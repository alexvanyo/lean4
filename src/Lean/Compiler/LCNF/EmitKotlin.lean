/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Lean FRO, LLC
-/
module

prelude
public import Lean.Compiler.LCNF.CompilerM
import Lean.Compiler.LCNF.EmitUtil
import Lean.Compiler.NameMangling
import Lean.Compiler.LCNF.PhaseExt
import Lean.Compiler.ExportAttr
import Lean.Compiler.ModPkgExt
import Lean.Compiler.LCNF.Internalize
import Lean.Compiler.InitAttr
public import Lean.Compiler.LCNF.ToImpureType
import Lean.Compiler.InlineAttrs
import Lean.Compiler.KotlinAttrs
import Lean.Compiler.LCNF.KotlinOwnership
import Lean.Structure
import Lean.Compiler.LCNF.Types
import Lean.Compiler.LCNF.MonoTypes
import Lean.Compiler.LCNF.FVarUtil
import Lean.Compiler.LCNF.ToImpure
import Lean.Compiler.LCNF.PushProj
import Lean.Compiler.LCNF.ElimDead
import Lean.Compiler.LCNF.SimpCase
import Lean.Compiler.LCNF.InferBorrow
import Lean.Compiler.LCNF.ExplicitBoxing
import Lean.Compiler.LCNF.ExplicitRC
import Lean.Compiler.LCNF.CoalesceRC
public import Lean.Compiler.KotlinSpec
import Lean.DocString.Extension
import Lean.AuxRecursor
import Std.Data.HashSet

namespace Lean.Compiler.LCNF

namespace Kotlin

open ImpureType

register_builtin_option compiler.kotlin.package : String := {
  defValue := ""
  descr := "package name for generated Kotlin source code"
}

register_builtin_option compiler.kotlin.preamble : String := {
  defValue := ""
  descr := "inline preamble content for generated Kotlin source code"
}

register_builtin_option compiler.kotlin.preamble_file : String := {
  defValue := ""
  descr := "path to file whose contents are prepended to generated Kotlin source code"
}

register_builtin_option compiler.kotlin.footer : String := {
  defValue := ""
  descr := "inline footer content for generated Kotlin source code"
}

register_builtin_option compiler.kotlin.footer_file : String := {
  defValue := ""
  descr := "path to file whose contents are appended to generated Kotlin source code"
}

register_builtin_option compiler.kotlin.topLevelModifiers : String := {
  defValue := "@PublishedApi internal"
  descr := "Kotlin modifiers for generated top-level functions"
}

register_builtin_option compiler.kotlin.pruneUnreachable : Bool := {
  defValue := false
  descr := "only emit declarations reachable from `@[kotlin_member]` declarations, `@[export]` \
    declarations, and generated names referenced from the preamble/footer"
}

/-- How a return inside `emitLoopExpansion` exits the inlined/loop body. -/
inductive LoopExit where
  | funcReturn
  | inlineAlias (fv : FVarId)
  | assignOnly (varName : String) (varTy : String)
  | assignAndBreak (varName : String) (varTy : String) (loopLbl : String)
  | breakOnly (loopLbl : String)
  | returnLbl (lbl : String) (varTy? : Option String)
  | threaded (successExit : LoopExit) (failureLbl : String)

/-- Destination of an inlined loop expansion (`emitLoopExpansion`). -/
inductive LoopDest where
  | tail
  | assign (fv : FVarId) (ty : Expr)
  | threaded (outerExit : LoopExit)

/-- State of a self-tail-recursive function whose body is being emitted as a `while (true)` loop. -/
structure LoopCtx where
  fnName : Name
  /-- Kotlin names holding the current value of each parameter. -/
  varNames : Array String
  /-- `true` if the parameter changes across iterations (emitted as `var`). -/
  variant : Array Bool
  loopLbl : String
  exit : LoopExit := .funcReturn

structure Context where
  modName : Name
  localDecls : Array (Decl .impure)
  otherModuleDecls : Array (Signature .impure) := #[]
  currFn : Name := default
  currParams : Array (Param .impure) := #[]
  declMap : Std.HashMap Name (Decl .impure) := {}
  /-- Class name when emitting a `@[kotlin_member]` declaration. -/
  currClass? : Option String := none
  loop? : Option LoopCtx := none
  /-- Kotlin type that function-level returns are cast to (from `@[kotlin_types]`). -/
  retCast? : Option String := none
  /-- The enclosing Kotlin function returns `Unit` (its Lean result type is `Unit`). -/
  retUnit : Bool := false
  /-- Ownership summaries (see `Ownership.analyze`). -/
  summaries : Std.HashMap Name Ownership.Summary := {}
  /--
  Result shape of the current `return` target when results identical to parameters are dropped
  (see `dropShape?`): such results are not returned, the caller reuses its argument.
  -/
  retShape? : Option Ownership.RetShape := none
  /-- Structures with `@[kotlin_class]`, by Kotlin type. -/
  classStructs : Std.HashMap String Name := {}
  /-- External cross-module declarations emitted as file-private helpers. -/
  extDeclNames : Std.HashSet Name := {}

structure State where
  buf : String := ""
  indent : Nat := 0
  varNames : Std.HashMap FVarId String := {}
  nameCounter : Nat := 0
  inlinedJps : Std.HashMap FVarId (FunDecl .impure) := {}
  /-- Non-inlined join points emitted sequentially (`decl`, optional `do..while(false)` label, param-is-aliased mask). -/
  blockJps : Std.HashMap FVarId (FunDecl .impure × Option String × Array Bool) := {}
  /-- Boolean variables (by Kotlin name) whose value is known in the current branch. -/
  knownBools : Std.HashMap String Bool := {}
  loopCounter : Nat := 0
  /--
  Partial applications of local declarations, keyed by the Kotlin name of the closure variable.
  Applications of these closures are beta-inlined instead of emitting a Kotlin closure.
  -/
  paps : Std.HashMap String (Name × Array (Arg .impure)) := {}
  /--
  `Prod.mk` values that are not materialized, keyed by the Kotlin name of the variable: the Kotlin
  expression of each component.
  -/
  tuples : Std.HashMap String (Array String) := {}
  /-- `Prod.mk` variables in `tuples` that have also been materialized as Kotlin `Pair` values. -/
  materializedTuples : Std.HashSet String := {}
  /-- `@[kotlin_class]` structure of Kotlin variables. -/
  nameStructs : Std.HashMap String Name := {}
  /--
  Fields of `@[kotlin_class]` values (by Kotlin lvalue, e.g. `keys` or `other.keys`) whose current
  value is known to be held by a Kotlin variable at this point of the emitted code. Writing that
  variable back to the field is skipped.
  -/
  fieldVals : Std.HashMap String String := {}
  /-- Variables used by the code being emitted, other than by RC instructions. -/
  used : Std.HashSet FVarId := {}
  /-- Kotlin variables read from a field of a `@[kotlin_class]` value: that value. -/
  projSrc : Std.HashMap String FVarId := {}
  /-- No-op field writes of in-place updates (see `collectWriteBacks`). -/
  writeBacks : Std.HashSet (FVarId × FVarId) := {}
  /-- Static Kotlin types of emitted variables and literals (by Kotlin expression/name). -/
  varTypes : Std.HashMap String String := {}
  /-- Kotlin expressions of Int origin that were converted to Long (by Kotlin variable name). -/
  intSources : Std.HashMap String String := {}
  /-- Precomputed variable aliases for the current function or loop body. -/
  aliases : Std.HashMap FVarId FVarId := {}
  /-- Recorded exit expressions for each loop label. -/
  loopExits : Std.HashMap String (Array String) := {}
  /-- Kotlin variables known to hold general ADT/structure `Array<Any?>` values (not `Pair`). -/
  adtVars : Std.HashSet String := {}

abbrev EmitM := ReaderT Context StateRefT State CompilerM

def emit (s : String) : EmitM Unit := do
  modify fun st => { st with buf := st.buf ++ s }

def emitIndent : EmitM Unit := do
  let indent := (← get).indent
  let spaces := String.ofList (List.replicate (indent * 4) ' ')
  emit spaces

def emitLn (s : String := "") : EmitM Unit := do
  if !s.isEmpty then
    emitIndent
    emit s
  emit "\n"

def withIndent (act : EmitM α) : EmitM α := do
  modify fun st => { st with indent := st.indent + 1 }
  let res ← act
  modify fun st => { st with indent := st.indent - 1 }
  return res

def captureBuf (act : EmitM Unit) : EmitM String := do
  let saved := (← get).buf
  modify fun st => { st with buf := "" }
  act
  let out := (← get).buf
  modify fun st => { st with buf := saved }
  return out

def splitLines (s : String) : List String :=
  let (acc, cur) := s.foldl (init := (([] : List String), "")) fun (acc, cur) c =>
    if c == '\n' then (cur :: acc, "") else (acc, cur.push c)
  (cur :: acc).reverse

partial def toKotlinType (ty : Expr) : String :=
  match ty with
  | ImpureType.bool => "Boolean"
  | ImpureType.uint8 => "UByte"
  | ImpureType.uint16 => "UShort"
  | ImpureType.uint32 => "UInt"
  | ImpureType.uint64 => "ULong"
  | ImpureType.usize => "Int"
  | ImpureType.int8 => "Byte"
  | ImpureType.int16 => "Short"
  | ImpureType.int32 => "Int"
  | ImpureType.int64 => "Long"
  | ImpureType.isize => "Int"
  | ImpureType.float => "Double"
  | ImpureType.float32 => "Float"
  | .const ``Bool _ | .const ``Decidable _ => "Boolean"
  | .const ``Nat _ => "java.math.BigInteger"
  | .const ``Unit _ | .const ``PUnit _ => "Unit"
  | .const ``String _ => "String"
  | .const ``ByteArray _ => "ByteArray"
  | .const ``FloatArray _ => "DoubleArray"
  | .app (.app (.const ``Prod _) a) b =>
    s!"Pair<{toKotlinType a}, {toKotlinType b}>"
  | .app (.const `jvmType _) (.lit (.strVal desc)) =>
    -- Types declared with `@[extern "kotlin:<Kotlin type>"]`.
    if desc.startsWith "kotlin:" then (desc.drop 7).toString else "Any?"
  | .forallE .. =>
    let rec go (e : Expr) (params : List String) : String :=
      match e with
      | .forallE _ d b _ =>
        let pTy := toKotlinType d
        go b (pTy :: params)
      | r =>
        let retTy := toKotlinType r
        let paramStr := String.intercalate ", " params.reverse
        s!"({paramStr}) -> {retTy}"
    go ty []
  | _ => "Any?"

def isRichMonoType (e : Expr) : Bool :=
  e.isForall || e.isConstOf ``Nat || e.isConstOf ``String || e.isConstOf ``ByteArray || e.isConstOf ``FloatArray || e.isAppOfArity ``Prod 2 ||
  e.isAppOf `jvmType

def isAdtMonoType (e : Expr) : Bool :=
  let fn := e.getAppFn
  fn.isConst && !fn.isConstOf ``lcAny && !fn.isConstOf ``lcErased && !fn.isConstOf ``Prod &&
    !fn.isConstOf ``Array && !fn.isConstOf ``ByteArray && !fn.isConstOf ``FloatArray &&
    !fn.isConstOf ``String && !fn.isConstOf ``Nat && !fn.isConstOf ``Int

/-- Parse `Pair<A, B>` into `(A, B)`. -/
partial def parseKotlinPairType? (s : String) : Option (String × String) :=
  if !s.startsWith "Pair<" || !s.endsWith ">" then none
  else
    let inner := ((s.drop 5).take (s.length - 6)).toString
    let rec go (cs : List Char) (depth : Nat) (cur : String) : Option (String × String) :=
      match cs with
      | [] => none
      | '<' :: rest => go rest (depth + 1) (cur.push '<')
      | '(' :: rest => go rest (depth + 1) (cur.push '(')
      | '>' :: rest => go rest (depth - 1) (cur.push '>')
      | ')' :: rest => go rest (depth - 1) (cur.push ')')
      | ',' :: ' ' :: rest =>
        if depth == 0 then some (cur, String.ofList rest)
        else go (' ' :: rest) depth (cur.push ',')
      | c :: rest => go rest depth (cur.push c)
    go inner.toList 0 ""

/-- Parse a Kotlin function type `(P1, P2, ...) -> R` into `(#[P1, P2, ...], R)`. -/
partial def parseKotlinFnType? (s : String) : Option (Array String × String) :=
  if !s.startsWith "(" then none
  else
    let rec go (cs : List Char) (depth : Nat) (cur : String) (params : Array String) : Option (Array String × String) :=
      match cs with
      | [] => none
      | '(' :: rest =>
        go rest (depth + 1) (if depth == 0 then cur else cur.push '(') params
      | ')' :: rest =>
        if depth == 0 then none
        else if depth == 1 then
          let params := if cur.isEmpty && params.isEmpty then params else params.push cur
          match rest with
          | ' ' :: '-' :: '>' :: ' ' :: retChars =>
            some (params, String.ofList retChars)
          | _ => none
        else
          go rest (depth - 1) (cur.push ')') params
      | ',' :: ' ' :: rest =>
        if depth == 1 then
          go rest 1 "" (params.push cur)
        else
          go (' ' :: rest) depth (cur.push ',') params
      | c :: rest =>
        go rest depth (cur.push c) params
    go s.toList 0 "" #[]

def defaultKotlinVal (ty : String) : String :=
  match ty with
  | "Boolean" => "false"
  | "Byte" => "(0).toByte()"
  | "Short" => "(0).toShort()"
  | "Int" => "0"
  | "Long" => "0L"
  | "UByte" => "(0u).toUByte()"
  | "UShort" => "(0u).toUShort()"
  | "UInt" => "0u"
  | "ULong" => "0uL"
  | "Double" => "0.0"
  | "Float" => "0.0f"
  | "String" => "\"\""
  | "java.math.BigInteger" | "BigInteger" => "java.math.BigInteger.ZERO"
  | "ByteArray" => "ByteArray(0)"
  | "DoubleArray" => "DoubleArray(0)"
  | "Unit" => "Unit"
  | _ => if ty.endsWith "?" then "null" else s!"(null as {ty})"

def recordVarType (name : String) (ty : String) : EmitM Unit := do
  if ty != "Any?" && ty != "_" then
    modify fun st => { st with varTypes := st.varTypes.insert name ty }

def isKotlinIntType (ty : String) : Bool :=
  ty == "Byte" || ty == "Short" || ty == "Int" || ty == "Long" ||
  ty == "UByte" || ty == "UShort" || ty == "UInt" || ty == "ULong"

def isBigIntType (ty : String) : Bool :=
  ty == "java.math.BigInteger" || ty == "BigInteger"

def extractNatLiteralDigits? (s : String) : Option String :=
  if s == "java.math.BigInteger.ZERO" then some "0"
  else if s == "java.math.BigInteger.ONE" then some "1"
  else if s == "java.math.BigInteger.TWO" then some "2"
  else if s == "java.math.BigInteger.TEN" then some "10"
  else if s.startsWith "java.math.BigInteger.valueOf(" && s.endsWith "L)" then
    let d := ((s.drop 29).take (s.length - 31)).toString
    if !d.isEmpty && d.all Char.isDigit then some d else none
  else if s.startsWith "java.math.BigInteger(\"" && s.endsWith "\")" then
    let d := ((s.drop 22).take (s.length - 24)).toString
    if !d.isEmpty && d.all Char.isDigit then some d else none
  else if !s.isEmpty && s.all Char.isDigit then some s
  else none

partial def stripOuterParens (s : String) : String := Id.run do
  let s := s.trimAscii.toString
  if s.startsWith "(" && s.endsWith ")" && s.length >= 2 then
    let mut depth := 0
    let mut matched := true
    let chars := s.toList
    for h : i in [:chars.length] do
      let c := chars[i]
      if c == '(' then depth := depth + 1
      else if c == ')' then
        depth := depth - 1
        if depth == 0 && i < chars.length - 1 then
          matched := false
          break
    if matched && depth == 0 then
      stripOuterParens ((s.drop 1).take (s.length - 2)).toString
    else
      s
  else
    s

def stripBigIntToInt? (s : String) : Option String := Id.run do
  let sClean := stripOuterParens s
  if sClean.startsWith "java.math.BigInteger.valueOf(" && sClean.endsWith ")" then
    let chars := sClean.toList
    let mut depth := 0
    let mut closingIdx? : Option Nat := none
    for h : i in [:chars.length] do
      let c := chars[i]
      if c == '(' then depth := depth + 1
      else if c == ')' then
        depth := depth - 1
        if depth == 0 then
          closingIdx? := some i
          break
    if closingIdx? == some (chars.length - 1) then
      let inner := stripOuterParens ((sClean.drop 29).take (sClean.length - 30)).toString
      if inner.endsWith ".size.toLong()" then
        some ((inner.take (inner.length - 14)).toString ++ ".size")
      else if inner.endsWith ".length.toLong()" then
        some ((inner.take (inner.length - 16)).toString ++ ".length")
      else if inner.endsWith ".toLong()" then
        let base := stripOuterParens (inner.take (inner.length - 9)).toString
        if base.endsWith ".toUInt()" then
          some (stripOuterParens (base.take (base.length - 9)).toString)
        else if base.endsWith ".toInt()" then
          some (stripOuterParens (base.take (base.length - 8)).toString)
        else
          some s!"({base}).toInt()"
      else
        some s!"({inner}).toInt()"
    else none
  else if let some d := extractNatLiteralDigits? sClean then
    if d.length <= 9 then some d else none
  else none

def kotlinIntConversionMethod (targetTy : String) : Option String :=
  match targetTy with
  | "Byte" => some "toByte()"
  | "Short" => some "toShort()"
  | "Int" => some "toInt()"
  | "Long" => some "toLong()"
  | "UByte" => some "toUByte()"
  | "UShort" => some "toUShort()"
  | "UInt" => some "toUInt()"
  | "ULong" => some "toULong()"
  | _ => none

def castIfNeeded (s : String) (targetTy : String) (knownTy? : Option String := none) : EmitM String := do
  if targetTy == "Any?" || targetTy == "_" then
    return s
  let vt := (← get).varTypes
  let actualTy? := match knownTy? with
    | some t => if t == "Any?" then vt[s]? else some t
    | none => vt[s]?
  if actualTy? == some targetTy || actualTy? == some "Nothing" || (actualTy?.map (s!"{·}?") == some targetTy) then
    return s
  if isKotlinIntType targetTy then
    if let some d := extractNatLiteralDigits? s then
      if d.length <= 9 then
        match targetTy with
        | "Int" => return d
        | "Long" => return s!"{d}L"
        | "UInt" => return s!"{d}u"
        | "ULong" => return s!"{d}uL"
        | "Byte" => return s!"({d}).toByte()"
        | "Short" => return s!"({d}).toShort()"
        | "UByte" => return s!"({d}u).toUByte()"
        | "UShort" => return s!"({d}u).toUShort()"
        | _ => pure ()
    if targetTy == "Int" then
      if let some inner := stripBigIntToInt? s then
        return inner
  let varTypes := (← get).varTypes
  let effActualTy? := actualTy?.orElse fun _ => varTypes[stripOuterParens s]?
  if let some actualTy := effActualTy? then
    if isKotlinIntType actualTy then
      if let some conv := kotlinIntConversionMethod targetTy then
        return s!"({s}).{conv}"
      if isBigIntType targetTy then
        if actualTy == "ULong" then
          return s!"(run \{ val l = ({s}).toLong(); if (l >= 0L) java.math.BigInteger.valueOf(l) else java.math.BigInteger.valueOf(l and Long.MAX_VALUE).setBit(63) })"
        else
          return s!"java.math.BigInteger.valueOf(({s}).toLong())"
    if isBigIntType actualTy then
      match targetTy with
      | "Byte" => return s!"({s}).toByte()"
      | "Short" => return s!"({s}).toShort()"
      | "Int" => return s!"({s}).toInt()"
      | "Long" => return s!"({s}).toLong()"
      | "UByte" => return s!"({s}).toByte().toUByte()"
      | "UShort" => return s!"({s}).toShort().toUShort()"
      | "UInt" => return s!"({s}).toInt().toUInt()"
      | "ULong" => return s!"({s}).toLong().toULong()"
      | "Double" => return s!"({s}).toDouble()"
      | "Float" => return s!"({s}).toFloat()"
      | _ => pure ()
  if isBigIntType targetTy then
    let sTrim := stripOuterParens s
    let isIntExpr := sTrim.endsWith ".toInt()" || sTrim.endsWith ".toByte()" || sTrim.endsWith ".toShort()" || sTrim.endsWith ".toLong()" ||
                     (!sTrim.isEmpty && sTrim.all (fun c => c.isDigit || c == '-'))
    if isIntExpr then
      return s!"java.math.BigInteger.valueOf(({s}).toLong())"
  return s!"({s} as {targetTy})"

def isKotlinKeyword (s : String) : Bool :=
  match s with
  | "as" | "break" | "class" | "continue" | "do" | "else" | "false" | "for" | "fun"
  | "if" | "in" | "interface" | "is" | "null" | "object" | "package" | "return"
  | "super" | "this" | "throw" | "true" | "try" | "typealias" | "typeof" | "val"
  | "var" | "when" | "while" => true
  | _ => false

def toPascalCase (s : String) : String :=
  if s.isEmpty then s
  else s!"{s.front.toUpper}{s.drop 1}"

def sanitizeIdent (s : String) : String :=
  s.foldl (init := "") fun acc c =>
    if c.isAlphanum || c == '_' then acc.push c else acc.push '_'

def getVarName (fvarId : FVarId) : EmitM String := do
  let mut f := fvarId
  for _ in [:8] do
    if let some r := (← get).aliases[f]? then
      if r != f then
        f := r
        continue
    break
  if let some name := (← get).varNames[f]? then
    return name
  let rawName := (← getBinderName f).eraseMacroScopes.toString
  let cleanName := sanitizeIdent rawName
  let cleanName :=
    if cleanName.isEmpty || cleanName.front.isDigit || cleanName.startsWith "_" then
      "v" ++ (if cleanName.startsWith "_" then cleanName else "_" ++ cleanName)
    else
      cleanName
  let count := (← get).nameCounter + 1
  let uniqueName := s!"{cleanName}_{count}"
  modify fun st => {
    st with
    varNames := st.varNames.insert f uniqueName
    nameCounter := count
  }
  return uniqueName

def setParamVarName (fvarId : FVarId) (name : String) : EmitM Unit := do
  modify fun st => { st with varNames := st.varNames.insert fvarId name }

def toKotlinFnName (n : Name) : String :=
  n.mangle (pre := "f_")

def toKotlinArg (arg : Arg .impure) : EmitM String := do
  match arg with
  | .fvar fvarId =>
    let n ← getVarName fvarId
    if let some b := (← get).knownBools[n]? then
      return if b then "true" else "false"
    return n
  | .erased => return "null"

def formatInt32 (n : UInt32) : String :=
  let signedVal : Int := if n.toNat > 2147483647 then (n.toNat : Int) - 4294967296 else (n.toNat : Int)
  if signedVal == -2147483648 then "Int.MIN_VALUE"
  else if signedVal < 0 then s!"({signedVal})" else s!"{signedVal}"

def formatUInt32 (n : UInt32) : String :=
  s!"{n.toNat}u"

def formatInt64 (n : UInt64) : String :=
  let signedVal : Int := if n.toNat > 9223372036854775807 then (n.toNat : Int) - 18446744073709551616 else (n.toNat : Int)
  if signedVal == -9223372036854775808 then "Long.MIN_VALUE"
  else if signedVal < 0 then s!"({signedVal}L)" else s!"{signedVal}L"

def formatUInt64 (n : UInt64) : String :=
  s!"{n.toNat}uL"

def stripToLong? (s : String) : Option String :=
  let sClean := stripOuterParens s
  if sClean.endsWith ".toLong()" && sClean.length >= 9 then
    some (sClean.take (sClean.length - 9)).toString
  else if sClean.endsWith ".toULong()" && sClean.length >= 10 then
    some (sClean.take (sClean.length - 10)).toString
  else
    none

def fromNatTo (s : String) (targetTy : String) : EmitM String := do
  if isKotlinIntType targetTy then
    if let some d := extractNatLiteralDigits? s then
      if d.length <= 9 then
        return ← castIfNeeded s targetTy
    let st ← get
    let inner? := match stripBigIntToInt? s with
      | some inner => some inner
      | none => st.intSources[s]?
    if let some inner := inner? then
      if targetTy == "Int" then return inner
      else return ← castIfNeeded inner targetTy (some "Int")
  if let some ty := (← get).varTypes[s]? then
    if isKotlinIntType ty || isBigIntType ty then
      return ← castIfNeeded s targetTy (some ty)
  let b ← castIfNeeded s "java.math.BigInteger"
  castIfNeeded b targetTy (some "java.math.BigInteger")

def formatNatLit (n : Nat) : String :=
  if n == 0 then "java.math.BigInteger.ZERO"
  else if n == 1 then "java.math.BigInteger.ONE"
  else if n == 2 then "java.math.BigInteger.TWO"
  else if n == 10 then "java.math.BigInteger.TEN"
  else if n <= 9223372036854775807 then s!"java.math.BigInteger.valueOf({n}L)"
  else s!"java.math.BigInteger(\"{n}\")"

def emitPrimitiveOp? (fn : Name) (args : Array (Arg .impure)) : EmitM (Option String) := do
  let fn := match fn with | .str p "_boxed" => p | _ => fn
  if fn == `lcUnreachable || fn == `lcUnreachable._redArg then
    return some "(error(\"unreachable code\") as Any?)"
  let fnStr := fn.toString
  if fn == `panic || fn == `panic._redArg || fn == `panicCore || fn == `panicCore._redArg ||
     fn == `panicWithPos || fn == `panicWithPos._redArg ||
     fn == `panicWithPosWithDecl || fn == `panicWithPosWithDecl._redArg ||
     fnStr.startsWith "panic._at_." || fnStr.startsWith "panicWithPos._at_." ||
     fnStr.startsWith "panicWithPosWithDecl._at_." then
    if args.isEmpty then
      return some "error(\"panic\")"
    let msg ← toKotlinArg args.back!
    return some s!"error({msg})"
  if (fn == ``Thunk.mk || fn == `Thunk.mk._redArg) && !args.isEmpty then
    let fStr ← toKotlinArg args.back!
    let pTy? := match ((← get).varTypes[fStr]?).bind parseKotlinFnType? with
      | some (#[pTy], _) => some pTy
      | _ => none
    let unitArg := if pTy? == some "Unit" then "Unit" else "null"
    let callExpr := Id.run do
      if let some pTy := pTy? then
        let pfx := s!"\{ p_0: {pTy} -> "
        if fStr.startsWith pfx then
          if fStr.endsWith ", p_0) }" then
            let inner := ((fStr.drop pfx.length).take (fStr.length - pfx.length - 8)).toString
            return s!"{inner}, {unitArg})"
          if fStr.endsWith "(p_0) }" then
            let inner := ((fStr.drop pfx.length).take (fStr.length - pfx.length - 7)).toString
            return s!"{inner}({unitArg})"
        return s!"({fStr})({unitArg})"
      return s!"({fStr} as (Any?) -> Any?)(null)"
    return some s!"lazy \{ {callExpr} }"
  if (fn == ``Thunk.pure || fn == `Thunk.pure._redArg) && !args.isEmpty then
    let a ← toKotlinArg args.back!
    return some s!"lazyOf({a})"
  if (fn == ``Thunk.get || fn == `Thunk.get._redArg) && !args.isEmpty then
    let t ← toKotlinArg args.back!
    return some s!"{← castIfNeeded t "Lazy<Any?>"}.value"
  if (fn == `Void.mk || fn == `Void.mk._redArg) && !args.isEmpty then
    return some "null"
  if (fn == `ST.Prim.mkRef || fn == `ST.Prim.mkRef._redArg) && args.size >= 2 then
    let a ← toKotlinArg args[args.size - 2]!
    return some s!"arrayOf<Any?>({a})"
  if (fn == `ST.Prim.Ref.get || fn == `ST.Prim.Ref.get._redArg ||
      fn == `ST.Prim.Ref.take || fn == `ST.Prim.Ref.take._redArg) && args.size >= 2 then
    let r ← castIfNeeded (← toKotlinArg args[args.size - 2]!) "Array<Any?>"
    return some s!"{r}[0]"
  if (fn == `ST.Prim.Ref.put || fn == `ST.Prim.Ref.put._redArg ||
      fn == `ST.Prim.Ref.set || fn == `ST.Prim.Ref.set._redArg) && args.size >= 3 then
    let r ← castIfNeeded (← toKotlinArg args[args.size - 3]!) "Array<Any?>"
    let a ← toKotlinArg args[args.size - 2]!
    return some s!"run \{ {r}[0] = {a} }"
  if (fn == `ST.Prim.Ref.swap || fn == `ST.Prim.Ref.swap._redArg) && args.size >= 3 then
    let r ← castIfNeeded (← toKotlinArg args[args.size - 3]!) "Array<Any?>"
    let a ← toKotlinArg args[args.size - 2]!
    return some s!"(run \{ val old = {r}[0]; {r}[0] = {a}; old })"
  if (fn == `ST.Prim.Ref.ptrEq || fn == `ST.Prim.Ref.ptrEq._redArg) && args.size >= 3 then
    let r1 ← toKotlinArg args[args.size - 3]!
    let r2 ← toKotlinArg args[args.size - 2]!
    return some s!"({r1} === {r2})"
  if (fn == ``UInt8.ofNatLT || fn == `UInt8.ofNatLT._redArg) && !args.isEmpty then
    return some (← fromNatTo (← toKotlinArg args[0]!) "UByte")
  if (fn == ``UInt16.ofNatLT || fn == `UInt16.ofNatLT._redArg) && !args.isEmpty then
    return some (← fromNatTo (← toKotlinArg args[0]!) "UShort")
  if (fn == ``UInt32.ofNatLT || fn == `UInt32.ofNatLT._redArg) && !args.isEmpty then
    return some (← fromNatTo (← toKotlinArg args[0]!) "UInt")
  if (fn == ``UInt64.ofNatLT || fn == `UInt64.ofNatLT._redArg) && !args.isEmpty then
    return some (← fromNatTo (← toKotlinArg args[0]!) "ULong")
  if (fn == ``USize.ofNatLT || fn == `USize.ofNatLT._redArg) && !args.isEmpty then
    return some (← fromNatTo (← toKotlinArg args[0]!) "Int")
  if (fn == `Int.divExact || fn == `Int.divExact._redArg) && args.size >= 2 then
    return some s!"({← castIfNeeded (← toKotlinArg args[0]!) "Int"} / {← castIfNeeded (← toKotlinArg args[1]!) "Int"})"
  if (fn == `Nat.divExact || fn == `Nat.divExact._redArg) && args.size >= 2 then
    let x ← castIfNeeded (← toKotlinArg args[0]!) "java.math.BigInteger"
    let y ← castIfNeeded (← toKotlinArg args[1]!) "java.math.BigInteger"
    return some s!"(if ({y} == java.math.BigInteger.ZERO) java.math.BigInteger.ZERO else ({x}).divide({y}))"
  if (fn == `dbgTrace || fn == `dbgTrace._redArg) && args.size >= 2 then
    let s ← castIfNeeded (← toKotlinArg args[args.size - 2]!) "String"
    let f ← toKotlinArg args.back!
    return some s!"(run \{ System.err.println({s}); ({f} as (Any?) -> Any?)(null) })"
  if (fn == `dbgTraceIfShared || fn == `dbgTraceIfShared._redArg) && !args.isEmpty then
    return some (← toKotlinArg args.back!)
  if (fn == `dbgStackTrace || fn == `dbgStackTrace._redArg) && !args.isEmpty then
    let f ← toKotlinArg args.back!
    return some s!"(run \{ Throwable().printStackTrace(System.err); ({f} as (Any?) -> Any?)(null) })"
  if (fn == `dbgSleep || fn == `dbgSleep._redArg) && args.size >= 2 then
    let ms ← castIfNeeded (← toKotlinArg args[args.size - 2]!) "UInt"
    let f ← toKotlinArg args.back!
    return some s!"(run \{ Thread.sleep(({ms}).toLong()); ({f} as (Any?) -> Any?)(null) })"
  if (fn == `isExclusiveUnsafe || fn == `isExclusiveUnsafe._redArg) && !args.isEmpty then
    return some "true"
  if (fn == `ptrAddrUnsafe || fn == `ptrAddrUnsafe._redArg) && !args.isEmpty then
    let a ← toKotlinArg args.back!
    return some s!"System.identityHashCode({a})"
  if fn == `System.Platform.getNumBits then
    return some "java.math.BigInteger.valueOf(32L)"
  if fn == `IO.initializing || fn == `IO.checkCanceled then
    return some "false"
  if fn == `IO.monoMsNow then
    return some "java.math.BigInteger.valueOf(System.nanoTime() / 1_000_000L)"
  if fn == `IO.monoNanosNow then
    return some "java.math.BigInteger.valueOf(System.nanoTime())"
  if fn == `IO.getNumHeartbeats then
    return some "java.math.BigInteger.ZERO"
  if fn == `IO.setNumHeartbeats || fn == `Runtime.forget || fn == `Runtime.forget._redArg ||
     fn == `Runtime.hold || fn == `Runtime.hold._redArg then
    return some "Unit"
  if (fn == `Runtime.markMultiThreaded || fn == `Runtime.markMultiThreaded._redArg ||
      fn == `Runtime.markPersistent || fn == `Runtime.markPersistent._redArg) && !args.isEmpty then
    let idx := if args.size >= 2 then args.size - 2 else 0
    return some (← toKotlinArg args[idx]!)
  if fn == `IO.getTID then
    return some "Thread.currentThread().id.toULong()"
  if fn == `IO.Process.getPID then
    return some "ProcessHandle.current().pid().toUInt()"
  if fn == `IO.getEnv && !args.isEmpty then
    let a0 ← castIfNeeded (← toKotlinArg args[0]!) "String"
    return some s!"(run \{ val v = System.getenv({a0}); if (v == null) arrayOf<Any?>(0) else arrayOf<Any?>(1, v) })"
  let defaultStdout := "arrayOf<Any?>(0, { _: Any? -> System.out.flush(); arrayOf<Any?>(0, arrayOf<Any?>(0)) }, { _: Any?, _: Any? -> arrayOf<Any?>(0, ByteArray(0)) }, { bs: Any?, _: Any? -> System.out.write(bs as ByteArray); arrayOf<Any?>(0, arrayOf<Any?>(0)) }, { _: Any? -> arrayOf<Any?>(0, \"\") }, { s: Any?, _: Any? -> kotlin.io.print(s as String); arrayOf<Any?>(0, arrayOf<Any?>(0)) }, { _: Any? -> false })"
  let defaultStderr := "arrayOf<Any?>(0, { _: Any? -> System.err.flush(); arrayOf<Any?>(0, arrayOf<Any?>(0)) }, { _: Any?, _: Any? -> arrayOf<Any?>(0, ByteArray(0)) }, { bs: Any?, _: Any? -> System.err.write(bs as ByteArray); arrayOf<Any?>(0, arrayOf<Any?>(0)) }, { _: Any? -> arrayOf<Any?>(0, \"\") }, { s: Any?, _: Any? -> System.err.print(s as String); arrayOf<Any?>(0, arrayOf<Any?>(0)) }, { _: Any? -> false })"
  let defaultStdin := "arrayOf<Any?>(0, { _: Any? -> arrayOf<Any?>(0, arrayOf<Any?>(0)) }, { n: Any?, _: Any? -> arrayOf<Any?>(0, System.`in`.readNBytes((n as Int).coerceAtLeast(0))) }, { _: Any?, _: Any? -> arrayOf<Any?>(0, arrayOf<Any?>(0)) }, { _: Any? -> arrayOf<Any?>(0, (readlnOrNull()?.plus(\"\\n\") ?: \"\")) }, { _: Any?, _: Any? -> arrayOf<Any?>(0, arrayOf<Any?>(0)) }, { _: Any? -> false })"
  if fn == `IO.getStdout then
    return some s!"((System.getProperties()[\"lean.stdout\"] as? Array<Any?>) ?: {defaultStdout})"
  if fn == `IO.setStdout && !args.isEmpty then
    let h ← toKotlinArg args[0]!
    return some s!"(run \{ val prev = (System.getProperties()[\"lean.stdout\"] as? Array<Any?>) ?: {defaultStdout}; System.getProperties()[\"lean.stdout\"] = {h}; prev })"
  if fn == `IO.getStderr then
    return some s!"((System.getProperties()[\"lean.stderr\"] as? Array<Any?>) ?: {defaultStderr})"
  if fn == `IO.setStderr && !args.isEmpty then
    let h ← toKotlinArg args[0]!
    return some s!"(run \{ val prev = (System.getProperties()[\"lean.stderr\"] as? Array<Any?>) ?: {defaultStderr}; System.getProperties()[\"lean.stderr\"] = {h}; prev })"
  if fn == `IO.getStdin then
    return some s!"((System.getProperties()[\"lean.stdin\"] as? Array<Any?>) ?: {defaultStdin})"
  if fn == `IO.setStdin && !args.isEmpty then
    let h ← toKotlinArg args[0]!
    return some s!"(run \{ val prev = (System.getProperties()[\"lean.stdin\"] as? Array<Any?>) ?: {defaultStdin}; System.getProperties()[\"lean.stdin\"] = {h}; prev })"
  if args.size == 1 then
    let a0 ← toKotlinArg args[0]!
    let a0Ty? := (← get).varTypes[a0]?
    match fn with
    -- Conversions to Int8 (Byte)
    | ``Int8.ofNat =>
      return some (← fromNatTo a0 "Byte")
    | ``Int8.ofInt | ``UInt8.toInt8 | ``Int16.toInt8 | ``Int32.toInt8 | ``Int64.toInt8 | ``ISize.toInt8 =>
      return some (if a0Ty? == some "Byte" then a0 else s!"({a0}).toByte()")
    | `Float.toInt8 | `Float32.toInt8 =>
      return some s!"({a0}).toInt().toByte()"
    | ``Bool.toInt8 =>
      return some s!"(if ({a0}) (1).toByte() else (0).toByte())"
    -- Conversions to Int16 (Short)
    | ``Int16.ofNat =>
      return some (← fromNatTo a0 "Short")
    | ``Int16.ofInt | ``UInt16.toInt16 | ``Int8.toInt16 | ``Int32.toInt16 | ``Int64.toInt16 | ``ISize.toInt16 =>
      return some (if a0Ty? == some "Short" then a0 else s!"({a0}).toShort()")
    | `Float.toInt16 | `Float32.toInt16 =>
      return some s!"({a0}).toInt().toShort()"
    | ``Bool.toInt16 =>
      return some s!"(if ({a0}) (1).toShort() else (0).toShort())"
    -- Conversions to Int32 / ISize / USize / Int (Int)
    | `Int.ofNat | ``Int32.ofNat | ``ISize.ofNat | ``USize.ofNat =>
      return some (← fromNatTo a0 "Int")
    | `Int8.toInt | `Int16.toInt | `Int32.toInt | `Int64.toInt
    | `Int.toInt32
    | ``Int32.ofInt | ``ISize.ofInt
    | ``UInt32.toInt32 | ``Int8.toInt32 | ``Int16.toInt32 | ``Int64.toInt32 | ``ISize.toInt32
    | ``Int32.toISize | ``Int64.toISize | ``UInt32.toUSize | ``UInt64.toUSize
    | `Float.toInt32 | `Float.toISize | `Float32.toInt32 | `Float32.toISize =>
      return some (if a0Ty? == some "Int" then a0 else s!"({a0}).toInt()")
    | `Float.toUSize | `Float32.toUSize =>
      return some s!"({a0}).toUInt().toInt()"
    | ``Bool.toInt32 | ``Bool.toISize | ``Bool.toUSize =>
      return some s!"(if ({a0}) 1 else 0)"
    -- Conversions to Nat (java.math.BigInteger)
    | ``UInt8.toNat =>
      return some s!"java.math.BigInteger.valueOf(({← castIfNeeded a0 "UByte"}).toLong())"
    | ``UInt16.toNat =>
      return some s!"java.math.BigInteger.valueOf(({← castIfNeeded a0 "UShort"}).toLong())"
    | ``UInt32.toNat | ``Char.toNat =>
      return some s!"java.math.BigInteger.valueOf(({← castIfNeeded a0 "UInt"}).toLong())"
    | ``USize.toNat =>
      return some s!"java.math.BigInteger.valueOf(({← castIfNeeded a0 "Int"}).toUInt().toLong())"
    | ``UInt64.toNat =>
      return some s!"(run \{ val l = ({← castIfNeeded a0 "ULong"}).toLong(); if (l >= 0L) java.math.BigInteger.valueOf(l) else java.math.BigInteger.valueOf(l and Long.MAX_VALUE).setBit(63) })"
    | `Int.toNat | ``Int32.toNatClampNeg | ``ISize.toNatClampNeg =>
      return some s!"java.math.BigInteger.valueOf(maxOf(0, {← castIfNeeded a0 "Int"}).toLong())"
    | ``Int8.toNatClampNeg =>
      return some s!"java.math.BigInteger.valueOf(maxOf(0, ({← castIfNeeded a0 "Byte"}).toInt()).toLong())"
    | ``Int16.toNatClampNeg =>
      return some s!"java.math.BigInteger.valueOf(maxOf(0, ({← castIfNeeded a0 "Short"}).toInt()).toLong())"
    | ``Int64.toNatClampNeg =>
      return some s!"java.math.BigInteger.valueOf(maxOf(0L, {← castIfNeeded a0 "Long"}))"
    | ``Bool.toNat =>
      return some s!"(if ({a0}) java.math.BigInteger.ONE else java.math.BigInteger.ZERO)"
    -- Conversions to Int64 (Long)
    | ``Int64.ofNat =>
      return some (← fromNatTo a0 "Long")
    | `Int.toInt64 | ``Int64.ofInt | ``UInt64.toInt64 | ``Int8.toInt64 | ``Int16.toInt64 | ``Int32.toInt64 | ``ISize.toInt64
    | `Float.toInt64 | `Float32.toInt64 =>
      return some (if a0Ty? == some "Long" then a0 else s!"({a0}).toLong()")
    | ``Bool.toInt64 =>
      return some s!"(if ({a0}) 1L else 0L)"
    -- Conversions to UInt8 (UByte)
    | ``UInt8.ofNat =>
      return some (← fromNatTo a0 "UByte")
    | ``Int8.toUInt8 | ``UInt16.toUInt8 | ``UInt32.toUInt8 | ``UInt64.toUInt8 | ``USize.toUInt8 =>
      return some (if a0Ty? == some "UByte" then a0 else s!"({a0}).toUByte()")
    | `Float.toUInt8 | `Float32.toUInt8 =>
      return some s!"({a0}).toUInt().toUByte()"
    | ``Bool.toUInt8 =>
      return some s!"(if ({a0}) (1u).toUByte() else (0u).toUByte())"
    -- Conversions to UInt16 (UShort)
    | ``UInt16.ofNat =>
      return some (← fromNatTo a0 "UShort")
    | ``Int16.toUInt16 | ``UInt8.toUInt16 | ``UInt32.toUInt16 | ``UInt64.toUInt16 | ``USize.toUInt16 =>
      return some (if a0Ty? == some "UShort" then a0 else s!"({a0}).toUShort()")
    | `Float.toUInt16 | `Float32.toUInt16 =>
      return some s!"({a0}).toUInt().toUShort()"
    | ``Bool.toUInt16 =>
      return some s!"(if ({a0}) (1u).toUShort() else (0u).toUShort())"
    -- Conversions to UInt32 (UInt) / Char
    | ``UInt32.ofNat | ``Char.ofNatAux =>
      return some (← fromNatTo a0 "UInt")
    | ``Int32.toUInt32 | ``UInt8.toUInt32 | ``UInt16.toUInt32 | ``UInt64.toUInt32 | ``USize.toUInt32
    | `Float.toUInt32 | `Float32.toUInt32 =>
      return some (if a0Ty? == some "UInt" then a0 else s!"({a0}).toUInt()")
    | ``Char.ofNat =>
      if let some d := extractNatLiteralDigits? a0 then
        if d.length <= 7 then
          return some s!"(run \{ val cp = {d}u; if (cp < 0xd800u || (cp > 0xdfffu && cp < 0x110000u)) cp else 0u })"
      return some s!"(run \{ val b = {← castIfNeeded a0 "java.math.BigInteger"}; val cp = if (b.bitLength() <= 21) b.toInt().toUInt() else 0x110000u; if (cp < 0xd800u || (cp > 0xdfffu && cp < 0x110000u)) cp else 0u })"
    | ``Char.utf8Size =>
      return some s!"java.math.BigInteger.valueOf((run \{ val cp = {← castIfNeeded a0 "UInt"}; if (cp <= 0x7Fu) 1L else if (cp <= 0x7FFu) 2L else if (cp <= 0xFFFFu) 3L else 4L }))"
    | ``Bool.toUInt32 =>
      return some s!"(if ({a0}) 1u else 0u)"
    -- Conversions to UInt64 (ULong)
    | ``UInt64.ofNat =>
      return some (← fromNatTo a0 "ULong")
    | ``Int64.toUInt64 | ``UInt8.toUInt64 | ``UInt16.toUInt64 | ``UInt32.toUInt64 | ``USize.toUInt64
    | `Float.toUInt64 | `Float32.toUInt64 =>
      return some (if a0Ty? == some "ULong" then a0 else s!"({a0}).toULong()")
    | ``Bool.toUInt64 =>
      return some s!"(if ({a0}) 1uL else 0uL)"
    -- Conversions to Float (Double)
    | `Float.ofNat | `Nat.toFloat =>
      return some (← fromNatTo a0 "Double")
    | `Float32.toFloat | `UInt8.toFloat | `UInt16.toFloat | `UInt32.toFloat | `UInt64.toFloat | `USize.toFloat
    | `Int8.toFloat | `Int16.toFloat | `Int32.toFloat | `Int64.toFloat | `ISize.toFloat
    | `Float.ofInt | `Int.toFloat =>
      return some (if a0Ty? == some "Double" then a0 else s!"({a0}).toDouble()")
    -- Conversions to Float32 (Float)
    | `Float32.ofNat | `Nat.toFloat32 =>
      return some (← fromNatTo a0 "Float")
    | `Float.toFloat32 | `UInt8.toFloat32 | `UInt16.toFloat32 | `UInt32.toFloat32 | `UInt64.toFloat32 | `USize.toFloat32
    | `Int8.toFloat32 | `Int16.toFloat32 | `Int32.toFloat32 | `Int64.toFloat32 | `ISize.toFloat32
    | `Float32.ofInt | `Int.toFloat32 =>
      return some (if a0Ty? == some "Float" then a0 else s!"({a0}).toFloat()")
    -- Unary Nat / Int
    | `Nat.pred =>
      return some s!"({← castIfNeeded a0 "java.math.BigInteger"}.subtract(java.math.BigInteger.ONE).max(java.math.BigInteger.ZERO))"
    | `Nat.log2 =>
      return some s!"java.math.BigInteger.valueOf(maxOf(0, ({← castIfNeeded a0 "java.math.BigInteger"}).bitLength() - 1).toLong())"
    | `Nat.reprFast | `Nat.repr | ``USize.repr | `Int.repr =>
      return some s!"({a0}).toString()"
    -- Unary negation
    | ``Int32.neg | ``Int64.neg | ``ISize.neg =>
      return some s!"(-{a0})"
    | `Int.neg =>
      return some s!"(-{← castIfNeeded a0 "Int"})"
    | `Int.negOfNat =>
      return some s!"(-{← fromNatTo a0 "Int"})"
    | `Int.negSucc =>
      return some s!"(-({← fromNatTo a0 "Int"} + 1))"
    | `Int.natAbs =>
      return some s!"java.math.BigInteger.valueOf(kotlin.math.abs(({← castIfNeeded a0 "Int"}).toLong()))"
    | `Int.decNonneg =>
      return some s!"({← castIfNeeded a0 "Int"} >= 0)"
    | ``Int8.neg =>
      return some s!"(-({a0}).toInt()).toByte()"
    | ``Int16.neg =>
      return some s!"(-({a0}).toInt()).toShort()"
    | ``UInt32.neg =>
      return some s!"(0u - {a0})"
    | ``UInt64.neg =>
      return some s!"(0uL - {a0})"
    | ``UInt8.neg =>
      return some s!"(0u - ({a0}).toUInt()).toUByte()"
    | ``UInt16.neg =>
      return some s!"(0u - ({a0}).toUInt()).toUShort()"
    | ``USize.neg =>
      return some s!"(-{a0})"
    -- Bitwise complement & log2 & abs
    | ``Int8.complement | ``Int16.complement | ``Int32.complement | ``Int64.complement | ``ISize.complement
    | ``UInt8.complement | ``UInt16.complement | ``UInt32.complement | ``UInt64.complement | ``USize.complement =>
      return some s!"{a0}.inv()"
    | `UInt8.log2 =>
      return some s!"(maxOf(0, 7 - ({← castIfNeeded a0 "UByte"}).countLeadingZeroBits())).toUByte()"
    | `UInt16.log2 =>
      return some s!"(maxOf(0, 15 - ({← castIfNeeded a0 "UShort"}).countLeadingZeroBits())).toUShort()"
    | `UInt32.log2 =>
      return some s!"(maxOf(0, 31 - ({← castIfNeeded a0 "UInt"}).countLeadingZeroBits())).toUInt()"
    | `UInt64.log2 =>
      return some s!"(maxOf(0, 63 - ({← castIfNeeded a0 "ULong"}).countLeadingZeroBits())).toULong()"
    | `USize.log2 =>
      return some s!"maxOf(0, 31 - ({← castIfNeeded a0 "Int"}).countLeadingZeroBits())"
    | `Int8.abs =>
      return some s!"(run \{ val v = {← castIfNeeded a0 "Byte"}; if (v < 0) (-v.toInt()).toByte() else v })"
    | `Int16.abs =>
      return some s!"(run \{ val v = {← castIfNeeded a0 "Short"}; if (v < 0) (-v.toInt()).toShort() else v })"
    | `Int32.abs | `ISize.abs =>
      return some s!"(run \{ val v = {← castIfNeeded a0 "Int"}; if (v < 0) -v else v })"
    | `Int64.abs =>
      return some s!"(run \{ val v = {← castIfNeeded a0 "Long"}; if (v < 0L) -v else v })"
    -- Unary Float (Double)
    | `Float.neg => return some s!"(-{← castIfNeeded a0 "Double"})"
    | `Float.abs => return some s!"kotlin.math.abs({← castIfNeeded a0 "Double"})"
    | `Float.sqrt => return some s!"kotlin.math.sqrt({← castIfNeeded a0 "Double"})"
    | `Float.sin => return some s!"kotlin.math.sin({← castIfNeeded a0 "Double"})"
    | `Float.cos => return some s!"kotlin.math.cos({← castIfNeeded a0 "Double"})"
    | `Float.tan => return some s!"kotlin.math.tan({← castIfNeeded a0 "Double"})"
    | `Float.asin => return some s!"kotlin.math.asin({← castIfNeeded a0 "Double"})"
    | `Float.acos => return some s!"kotlin.math.acos({← castIfNeeded a0 "Double"})"
    | `Float.atan => return some s!"kotlin.math.atan({← castIfNeeded a0 "Double"})"
    | `Float.sinh => return some s!"kotlin.math.sinh({← castIfNeeded a0 "Double"})"
    | `Float.cosh => return some s!"kotlin.math.cosh({← castIfNeeded a0 "Double"})"
    | `Float.tanh => return some s!"kotlin.math.tanh({← castIfNeeded a0 "Double"})"
    | `Float.asinh => return some s!"kotlin.math.asinh({← castIfNeeded a0 "Double"})"
    | `Float.acosh => return some s!"kotlin.math.acosh({← castIfNeeded a0 "Double"})"
    | `Float.atanh => return some s!"kotlin.math.atanh({← castIfNeeded a0 "Double"})"
    | `Float.exp => return some s!"kotlin.math.exp({← castIfNeeded a0 "Double"})"
    | `Float.exp2 => return some s!"Math.pow(2.0, {← castIfNeeded a0 "Double"})"
    | `Float.log => return some s!"kotlin.math.ln({← castIfNeeded a0 "Double"})"
    | `Float.log2 => return some s!"kotlin.math.log2({← castIfNeeded a0 "Double"})"
    | `Float.log10 => return some s!"kotlin.math.log10({← castIfNeeded a0 "Double"})"
    | `Float.cbrt => return some s!"Math.cbrt({← castIfNeeded a0 "Double"})"
    | `Float.floor => return some s!"kotlin.math.floor({← castIfNeeded a0 "Double"})"
    | `Float.ceil => return some s!"kotlin.math.ceil({← castIfNeeded a0 "Double"})"
    | `Float.round => return some s!"kotlin.math.round({← castIfNeeded a0 "Double"})"
    | `Float.isNaN => return some s!"({← castIfNeeded a0 "Double"}).isNaN()"
    | `Float.isFinite => return some s!"({← castIfNeeded a0 "Double"}).isFinite()"
    | `Float.isInf => return some s!"({← castIfNeeded a0 "Double"}).isInfinite()"
    | `Float.toString => return some s!"({← castIfNeeded a0 "Double"}).toString()"
    | `Float.toBits => return some s!"({← castIfNeeded a0 "Double"}).toRawBits().toULong()"
    | `Float.ofBits => return some s!"Double.fromBits(({← castIfNeeded a0 "ULong"}).toLong())"
    -- Unary Float32 (Float)
    | `Float32.neg => return some s!"(-{← castIfNeeded a0 "Float"})"
    | `Float32.abs => return some s!"kotlin.math.abs({← castIfNeeded a0 "Float"})"
    | `Float32.sqrt => return some s!"kotlin.math.sqrt({← castIfNeeded a0 "Float"})"
    | `Float32.sin => return some s!"kotlin.math.sin({← castIfNeeded a0 "Float"})"
    | `Float32.cos => return some s!"kotlin.math.cos({← castIfNeeded a0 "Float"})"
    | `Float32.tan => return some s!"kotlin.math.tan({← castIfNeeded a0 "Float"})"
    | `Float32.asin => return some s!"kotlin.math.asin({← castIfNeeded a0 "Float"})"
    | `Float32.acos => return some s!"kotlin.math.acos({← castIfNeeded a0 "Float"})"
    | `Float32.atan => return some s!"kotlin.math.atan({← castIfNeeded a0 "Float"})"
    | `Float32.sinh => return some s!"kotlin.math.sinh({← castIfNeeded a0 "Float"})"
    | `Float32.cosh => return some s!"kotlin.math.cosh({← castIfNeeded a0 "Float"})"
    | `Float32.tanh => return some s!"kotlin.math.tanh({← castIfNeeded a0 "Float"})"
    | `Float32.asinh => return some s!"kotlin.math.asinh({← castIfNeeded a0 "Float"})"
    | `Float32.acosh => return some s!"kotlin.math.acosh({← castIfNeeded a0 "Float"})"
    | `Float32.atanh => return some s!"kotlin.math.atanh({← castIfNeeded a0 "Float"})"
    | `Float32.exp => return some s!"kotlin.math.exp({← castIfNeeded a0 "Float"})"
    | `Float32.exp2 => return some s!"Math.pow(2.0, ({← castIfNeeded a0 "Float"}).toDouble()).toFloat()"
    | `Float32.log => return some s!"kotlin.math.ln({← castIfNeeded a0 "Float"})"
    | `Float32.log2 => return some s!"kotlin.math.log2({← castIfNeeded a0 "Float"})"
    | `Float32.log10 => return some s!"kotlin.math.log10({← castIfNeeded a0 "Float"})"
    | `Float32.cbrt => return some s!"Math.cbrt(({← castIfNeeded a0 "Float"}).toDouble()).toFloat()"
    | `Float32.floor => return some s!"kotlin.math.floor({← castIfNeeded a0 "Float"})"
    | `Float32.ceil => return some s!"kotlin.math.ceil({← castIfNeeded a0 "Float"})"
    | `Float32.round => return some s!"kotlin.math.round({← castIfNeeded a0 "Float"})"
    | `Float32.isNaN => return some s!"({← castIfNeeded a0 "Float"}).isNaN()"
    | `Float32.isFinite => return some s!"({← castIfNeeded a0 "Float"}).isFinite()"
    | `Float32.isInf => return some s!"({← castIfNeeded a0 "Float"}).isInfinite()"
    | `Float32.toString => return some s!"({← castIfNeeded a0 "Float"}).toString()"
    | `Float32.toBits => return some s!"({← castIfNeeded a0 "Float"}).toRawBits().toUInt()"
    | `Float32.ofBits => return some s!"Float.fromBits(({← castIfNeeded a0 "UInt"}).toInt())"
    -- Unary String
    | `String.length | `String.Internal.length => return some s!"java.math.BigInteger.valueOf(({← castIfNeeded a0 "String"}).length.toLong())"
    | `String.utf8ByteSize => return some s!"java.math.BigInteger.valueOf(({← castIfNeeded a0 "String"}).encodeToByteArray().size.toLong())"
    | `String.isEmpty | `String.Internal.isEmpty => return some s!"({← castIfNeeded a0 "String"}).isEmpty()"
    | `String.hash =>
      return some s!"({← castIfNeeded a0 "String"}).encodeToByteArray().contentHashCode().toULong()"
    | `String.Slice.hash =>
      return some s!"(run \{ val sl = {a0} as Array<*>; val s = (sl[1] as String).encodeToByteArray(); val b = (sl[2] as java.math.BigInteger).toInt().coerceIn(0, s.size); val e = (sl[3] as java.math.BigInteger).toInt().coerceIn(b, s.size); s.copyOfRange(b, e).contentHashCode().toULong() })"
    | `String.singleton => return some s!"Character.toString(({← castIfNeeded a0 "UInt"}).toInt())"
    | `String.ofList | `String.mk | `List.asString =>
      return some s!"buildString \{ var cur: Any? = {a0}; while (((cur as Array<*>)[0] as Int) == 1) \{ appendCodePoint(((cur as Array<*>)[1] as UInt).toInt()); cur = (cur as Array<*>)[2] } }"
    | `String.toList | `String.data =>
      return some s!"({← castIfNeeded a0 "String"}).codePoints().toArray().foldRight(arrayOf<Any?>(0) as Any?) \{ cp, acc -> arrayOf<Any?>(1, cp.toUInt(), acc) }"
    | `String.toUTF8 | `String.toByteArray =>
      return some s!"({← castIfNeeded a0 "String"}).encodeToByteArray()"
    | `String.fromUTF8! =>
      return some s!"({← castIfNeeded a0 "ByteArray"}).decodeToString()"
    | `String.markLinear => return some a0
    -- Unary ByteArray
    | `ByteArray.emptyWithCapacity => return some "ByteArray(0)"
    | `ByteArray.size => return some s!"java.math.BigInteger.valueOf(({← castIfNeeded a0 "ByteArray"}).size.toLong())"
    | `ByteArray.usize => return some s!"({← castIfNeeded a0 "ByteArray"}).size"
    | `ByteArray.isEmpty => return some s!"({← castIfNeeded a0 "ByteArray"}).isEmpty()"
    | `ByteArray.hash => return some s!"({← castIfNeeded a0 "ByteArray"}).contentHashCode().toULong()"
    | `ByteArray.markLinear => return some s!"({← castIfNeeded a0 "ByteArray"}).copyOf()"
    | `ByteArray.validateUTF8 =>
      return some s!"(try \{ java.nio.charset.StandardCharsets.UTF_8.newDecoder().decode(java.nio.ByteBuffer.wrap({← castIfNeeded a0 "ByteArray"})); true } catch (_: Throwable) \{ false })"
    -- Unary FloatArray
    | `FloatArray.emptyWithCapacity => return some "DoubleArray(0)"
    | `FloatArray.size => return some s!"java.math.BigInteger.valueOf(({← castIfNeeded a0 "DoubleArray"}).size.toLong())"
    | `FloatArray.usize => return some s!"({← castIfNeeded a0 "DoubleArray"}).size"
    | `FloatArray.isEmpty => return some s!"({← castIfNeeded a0 "DoubleArray"}).isEmpty()"
    | `FloatArray.markLinear => return some s!"({← castIfNeeded a0 "DoubleArray"}).copyOf()"
    | _ =>
      if fn.isStr && (fn.getString! == "ofNat" || fn.getString! == "toUInt64") then
        if a0Ty? == some "Long" then return some a0
        if a0Ty? == some "Int" then return some s!"{a0}.toLong()"
        if a0Ty? == some "java.math.BigInteger" then return some s!"({a0}).toLong()"
        return some s!"({a0} as Number).toLong()"
      if fn.isStr && fn.getString! == "toUInt32" then
        if a0Ty? == some "Int" then return some a0
        if a0Ty? == some "Long" then return some s!"{a0}.toInt()"
        if a0Ty? == some "java.math.BigInteger" then return some s!"({a0}).toInt()"
        return some s!"({a0} as Number).toInt()"
      return none
  if args.isEmpty then
    match fn with
    | `ByteArray.empty => return some "ByteArray(0)"
    | `FloatArray.empty => return some "DoubleArray(0)"
    | _ => return none
  if args.size == 2 then
    let a0 ← toKotlinArg args[0]!
    let a1 ← toKotlinArg args[1]!
    let a1Int ← do
      if (← get).varTypes[a1]? == some "Int" then
        pure a1
      else if let some d := extractNatLiteralDigits? a1 then
        if d.length <= 9 then pure d else pure s!"({a1}).toInt()"
      else if let some src := (← get).intSources[a1]? then
        pure src
      else if let some intExpr := stripToLong? a1 then
        pure intExpr
      else if a1.endsWith "uL" && a1.length > 2 && (a1.take (a1.length - 2)).all Char.isDigit then
        pure (a1.take (a1.length - 2)).toString
      else if (a1.endsWith "L" || a1.endsWith "u") && a1.length > 1 && (a1.take (a1.length - 1)).all Char.isDigit then
        pure (a1.take (a1.length - 1)).toString
      else
        pure s!"({a1}).toInt()"
    match fn with
    | `mixHash =>
      let h1 ← castIfNeeded a0 "ULong"
      let h2 ← castIfNeeded a1 "ULong"
      return some s!"(run \{ val k = {h2} * 0xc6a4a7935bd1e995uL; val k2 = (k xor (k shr 47)) * 0xc6a4a7935bd1e995uL; ({h1} xor k2) * 0xc6a4a7935bd1e995uL })"
    -- Signed Int32 / ISize / USize (Int)
    | ``Int32.add | ``ISize.add | ``USize.add =>
      return some s!"({← castIfNeeded a0 "Int"} + {← castIfNeeded a1 "Int"})"
    | ``Int32.sub | ``ISize.sub | ``USize.sub =>
      return some s!"({← castIfNeeded a0 "Int"} - {← castIfNeeded a1 "Int"})"
    | ``Int32.mul | ``ISize.mul | ``USize.mul =>
      return some s!"({← castIfNeeded a0 "Int"} * {← castIfNeeded a1 "Int"})"
    | ``Int32.div | ``ISize.div | ``USize.div =>
      return some s!"({← castIfNeeded a0 "Int"} / {← castIfNeeded a1 "Int"})"
    | ``Int32.mod | ``ISize.mod | ``USize.mod =>
      return some s!"({← castIfNeeded a0 "Int"} % {← castIfNeeded a1 "Int"})"
    | ``Int32.land | ``ISize.land | ``USize.land =>
      return some s!"({← castIfNeeded a0 "Int"} and {← castIfNeeded a1 "Int"})"
    | ``Int32.lor | ``ISize.lor | ``USize.lor =>
      return some s!"({← castIfNeeded a0 "Int"} or {← castIfNeeded a1 "Int"})"
    | ``Int32.xor | ``ISize.xor | ``USize.xor =>
      return some s!"({← castIfNeeded a0 "Int"} xor {← castIfNeeded a1 "Int"})"
    | ``Int32.shiftLeft | ``ISize.shiftLeft | ``USize.shiftLeft =>
      return some s!"({← castIfNeeded a0 "Int"} shl {a1Int})"
    | ``Int32.shiftRight | ``ISize.shiftRight =>
      return some s!"({← castIfNeeded a0 "Int"} shr {a1Int})"
    | ``USize.shiftRight =>
      return some s!"({← castIfNeeded a0 "Int"} ushr {a1Int})"
    | ``Int32.decEq | ``ISize.decEq | ``USize.decEq =>
      return some s!"({← castIfNeeded a0 "Int"} == {← castIfNeeded a1 "Int"})"
    | ``Int32.decLt | ``ISize.decLt | ``USize.decLt =>
      return some s!"({← castIfNeeded a0 "Int"} < {← castIfNeeded a1 "Int"})"
    | ``Int32.decLe | ``ISize.decLe | ``USize.decLe =>
      return some s!"({← castIfNeeded a0 "Int"} <= {← castIfNeeded a1 "Int"})"

    -- Signed Int64 (Long)
    | ``Int64.add => return some s!"({← castIfNeeded a0 "Long"} + {← castIfNeeded a1 "Long"})"
    | ``Int64.sub => return some s!"({← castIfNeeded a0 "Long"} - {← castIfNeeded a1 "Long"})"
    | ``Int64.mul => return some s!"({← castIfNeeded a0 "Long"} * {← castIfNeeded a1 "Long"})"
    | ``Int64.div => return some s!"({← castIfNeeded a0 "Long"} / {← castIfNeeded a1 "Long"})"
    | ``Int64.mod => return some s!"({← castIfNeeded a0 "Long"} % {← castIfNeeded a1 "Long"})"
    | ``Int64.land => return some s!"({← castIfNeeded a0 "Long"} and {← castIfNeeded a1 "Long"})"
    | ``Int64.lor => return some s!"({← castIfNeeded a0 "Long"} or {← castIfNeeded a1 "Long"})"
    | ``Int64.xor => return some s!"({← castIfNeeded a0 "Long"} xor {← castIfNeeded a1 "Long"})"
    | ``Int64.shiftLeft => return some s!"({← castIfNeeded a0 "Long"} shl {a1Int})"
    | ``Int64.shiftRight => return some s!"({← castIfNeeded a0 "Long"} shr {a1Int})"
    | ``Int64.decEq => return some s!"({← castIfNeeded a0 "Long"} == {← castIfNeeded a1 "Long"})"
    | ``Int64.decLt => return some s!"({← castIfNeeded a0 "Long"} < {← castIfNeeded a1 "Long"})"
    | ``Int64.decLe => return some s!"({← castIfNeeded a0 "Long"} <= {← castIfNeeded a1 "Long"})"

    -- Unsigned UInt32 (UInt)
    | ``UInt32.add => return some s!"({← castIfNeeded a0 "UInt"} + {← castIfNeeded a1 "UInt"})"
    | ``UInt32.sub => return some s!"({← castIfNeeded a0 "UInt"} - {← castIfNeeded a1 "UInt"})"
    | ``UInt32.mul => return some s!"({← castIfNeeded a0 "UInt"} * {← castIfNeeded a1 "UInt"})"
    | ``UInt32.div => return some s!"({← castIfNeeded a0 "UInt"} / {← castIfNeeded a1 "UInt"})"
    | ``UInt32.mod => return some s!"({← castIfNeeded a0 "UInt"} % {← castIfNeeded a1 "UInt"})"
    | ``UInt32.land => return some s!"({← castIfNeeded a0 "UInt"} and {← castIfNeeded a1 "UInt"})"
    | ``UInt32.lor => return some s!"({← castIfNeeded a0 "UInt"} or {← castIfNeeded a1 "UInt"})"
    | ``UInt32.xor | `UInt32.lxor => return some s!"({← castIfNeeded a0 "UInt"} xor {← castIfNeeded a1 "UInt"})"
    | ``UInt32.shiftLeft => return some s!"({← castIfNeeded a0 "UInt"} shl {a1Int})"
    | ``UInt32.shiftRight => return some s!"({← castIfNeeded a0 "UInt"} shr {a1Int})"
    | ``UInt32.decEq => return some s!"({← castIfNeeded a0 "UInt"} == {← castIfNeeded a1 "UInt"})"
    | ``UInt32.decLt => return some s!"({← castIfNeeded a0 "UInt"} < {← castIfNeeded a1 "UInt"})"
    | ``UInt32.decLe => return some s!"({← castIfNeeded a0 "UInt"} <= {← castIfNeeded a1 "UInt"})"

    -- Unsigned UInt64 (ULong)
    | ``UInt64.add => return some s!"({← castIfNeeded a0 "ULong"} + {← castIfNeeded a1 "ULong"})"
    | ``UInt64.sub => return some s!"({← castIfNeeded a0 "ULong"} - {← castIfNeeded a1 "ULong"})"
    | ``UInt64.mul => return some s!"({← castIfNeeded a0 "ULong"} * {← castIfNeeded a1 "ULong"})"
    | ``UInt64.div => return some s!"({← castIfNeeded a0 "ULong"} / {← castIfNeeded a1 "ULong"})"
    | ``UInt64.mod => return some s!"({← castIfNeeded a0 "ULong"} % {← castIfNeeded a1 "ULong"})"
    | ``UInt64.land => return some s!"({← castIfNeeded a0 "ULong"} and {← castIfNeeded a1 "ULong"})"
    | ``UInt64.lor => return some s!"({← castIfNeeded a0 "ULong"} or {← castIfNeeded a1 "ULong"})"
    | ``UInt64.xor | `UInt64.lxor => return some s!"({← castIfNeeded a0 "ULong"} xor {← castIfNeeded a1 "ULong"})"
    | ``UInt64.shiftLeft => return some s!"({← castIfNeeded a0 "ULong"} shl {a1Int})"
    | ``UInt64.shiftRight => return some s!"({← castIfNeeded a0 "ULong"} shr {a1Int})"
    | ``UInt64.decEq => return some s!"({← castIfNeeded a0 "ULong"} == {← castIfNeeded a1 "ULong"})"
    | ``UInt64.decLt => return some s!"({← castIfNeeded a0 "ULong"} < {← castIfNeeded a1 "ULong"})"
    | ``UInt64.decLe => return some s!"({← castIfNeeded a0 "ULong"} <= {← castIfNeeded a1 "ULong"})"

    -- Signed Int8 (Byte) & Int16 (Short)
    | ``Int8.add => return some s!"(({← castIfNeeded a0 "Byte"}.toInt() + {← castIfNeeded a1 "Byte"}.toInt()).toByte())"
    | ``Int8.sub => return some s!"(({← castIfNeeded a0 "Byte"}.toInt() - {← castIfNeeded a1 "Byte"}.toInt()).toByte())"
    | ``Int8.mul => return some s!"(({← castIfNeeded a0 "Byte"}.toInt() * {← castIfNeeded a1 "Byte"}.toInt()).toByte())"
    | ``Int8.div => return some s!"(({← castIfNeeded a0 "Byte"}.toInt() / {← castIfNeeded a1 "Byte"}.toInt()).toByte())"
    | ``Int8.mod => return some s!"(({← castIfNeeded a0 "Byte"}.toInt() % {← castIfNeeded a1 "Byte"}.toInt()).toByte())"
    | ``Int8.land => return some s!"(({← castIfNeeded a0 "Byte"}.toInt() and {← castIfNeeded a1 "Byte"}.toInt()).toByte())"
    | ``Int8.lor => return some s!"(({← castIfNeeded a0 "Byte"}.toInt() or {← castIfNeeded a1 "Byte"}.toInt()).toByte())"
    | ``Int8.xor => return some s!"(({← castIfNeeded a0 "Byte"}.toInt() xor {← castIfNeeded a1 "Byte"}.toInt()).toByte())"
    | ``Int8.shiftLeft => return some s!"(({← castIfNeeded a0 "Byte"}.toInt() shl {a1Int}).toByte())"
    | ``Int8.shiftRight => return some s!"(({← castIfNeeded a0 "Byte"}.toInt() shr {a1Int}).toByte())"
    | ``Int8.decEq => return some s!"({← castIfNeeded a0 "Byte"} == {← castIfNeeded a1 "Byte"})"
    | ``Int8.decLt => return some s!"({← castIfNeeded a0 "Byte"} < {← castIfNeeded a1 "Byte"})"
    | ``Int8.decLe => return some s!"({← castIfNeeded a0 "Byte"} <= {← castIfNeeded a1 "Byte"})"

    | ``Int16.add => return some s!"(({← castIfNeeded a0 "Short"}.toInt() + {← castIfNeeded a1 "Short"}.toInt()).toShort())"
    | ``Int16.sub => return some s!"(({← castIfNeeded a0 "Short"}.toInt() - {← castIfNeeded a1 "Short"}.toInt()).toShort())"
    | ``Int16.mul => return some s!"(({← castIfNeeded a0 "Short"}.toInt() * {← castIfNeeded a1 "Short"}.toInt()).toShort())"
    | ``Int16.div => return some s!"(({← castIfNeeded a0 "Short"}.toInt() / {← castIfNeeded a1 "Short"}.toInt()).toShort())"
    | ``Int16.mod => return some s!"(({← castIfNeeded a0 "Short"}.toInt() % {← castIfNeeded a1 "Short"}.toInt()).toShort())"
    | ``Int16.land => return some s!"(({← castIfNeeded a0 "Short"}.toInt() and {← castIfNeeded a1 "Short"}.toInt()).toShort())"
    | ``Int16.lor => return some s!"(({← castIfNeeded a0 "Short"}.toInt() or {← castIfNeeded a1 "Short"}.toInt()).toShort())"
    | ``Int16.xor => return some s!"(({← castIfNeeded a0 "Short"}.toInt() xor {← castIfNeeded a1 "Short"}.toInt()).toShort())"
    | ``Int16.shiftLeft => return some s!"(({← castIfNeeded a0 "Short"}.toInt() shl {a1Int}).toShort())"
    | ``Int16.shiftRight => return some s!"(({← castIfNeeded a0 "Short"}.toInt() shr {a1Int}).toShort())"
    | ``Int16.decEq => return some s!"({← castIfNeeded a0 "Short"} == {← castIfNeeded a1 "Short"})"
    | ``Int16.decLt => return some s!"({← castIfNeeded a0 "Short"} < {← castIfNeeded a1 "Short"})"
    | ``Int16.decLe => return some s!"({← castIfNeeded a0 "Short"} <= {← castIfNeeded a1 "Short"})"

    -- Unsigned UInt8 (UByte) & UInt16 (UShort)
    | ``UInt8.add => return some s!"(({← castIfNeeded a0 "UByte"}.toUInt() + {← castIfNeeded a1 "UByte"}.toUInt()).toUByte())"
    | ``UInt8.sub => return some s!"(({← castIfNeeded a0 "UByte"}.toUInt() - {← castIfNeeded a1 "UByte"}.toUInt()).toUByte())"
    | ``UInt8.mul => return some s!"(({← castIfNeeded a0 "UByte"}.toUInt() * {← castIfNeeded a1 "UByte"}.toUInt()).toUByte())"
    | ``UInt8.div => return some s!"(({← castIfNeeded a0 "UByte"}.toUInt() / {← castIfNeeded a1 "UByte"}.toUInt()).toUByte())"
    | ``UInt8.mod => return some s!"(({← castIfNeeded a0 "UByte"}.toUInt() % {← castIfNeeded a1 "UByte"}.toUInt()).toUByte())"
    | ``UInt8.land => return some s!"({← castIfNeeded a0 "UByte"} and {← castIfNeeded a1 "UByte"})"
    | ``UInt8.lor => return some s!"({← castIfNeeded a0 "UByte"} or {← castIfNeeded a1 "UByte"})"
    | ``UInt8.xor => return some s!"({← castIfNeeded a0 "UByte"} xor {← castIfNeeded a1 "UByte"})"
    | ``UInt8.shiftLeft => return some s!"(({← castIfNeeded a0 "UByte"}.toUInt() shl {a1Int}).toUByte())"
    | ``UInt8.shiftRight => return some s!"(({← castIfNeeded a0 "UByte"}.toUInt() shr {a1Int}).toUByte())"
    | ``UInt8.decEq => return some s!"({← castIfNeeded a0 "UByte"} == {← castIfNeeded a1 "UByte"})"
    | ``UInt8.decLt => return some s!"({← castIfNeeded a0 "UByte"} < {← castIfNeeded a1 "UByte"})"
    | ``UInt8.decLe => return some s!"({← castIfNeeded a0 "UByte"} <= {← castIfNeeded a1 "UByte"})"

    | ``UInt16.add => return some s!"(({← castIfNeeded a0 "UShort"}.toUInt() + {← castIfNeeded a1 "UShort"}.toUInt()).toUShort())"
    | ``UInt16.sub => return some s!"(({← castIfNeeded a0 "UShort"}.toUInt() - {← castIfNeeded a1 "UShort"}.toUInt()).toUShort())"
    | ``UInt16.mul => return some s!"(({← castIfNeeded a0 "UShort"}.toUInt() * {← castIfNeeded a1 "UShort"}.toUInt()).toUShort())"
    | ``UInt16.div => return some s!"(({← castIfNeeded a0 "UShort"}.toUInt() / {← castIfNeeded a1 "UShort"}.toUInt()).toUShort())"
    | ``UInt16.mod => return some s!"(({← castIfNeeded a0 "UShort"}.toUInt() % {← castIfNeeded a1 "UShort"}.toUInt()).toUShort())"
    | ``UInt16.land => return some s!"({← castIfNeeded a0 "UShort"} and {← castIfNeeded a1 "UShort"})"
    | ``UInt16.lor => return some s!"({← castIfNeeded a0 "UShort"} or {← castIfNeeded a1 "UShort"})"
    | ``UInt16.xor => return some s!"({← castIfNeeded a0 "UShort"} xor {← castIfNeeded a1 "UShort"})"
    | ``UInt16.shiftLeft => return some s!"(({← castIfNeeded a0 "UShort"}.toUInt() shl {a1Int}).toUShort())"
    | ``UInt16.shiftRight => return some s!"(({← castIfNeeded a0 "UShort"}.toUInt() shr {a1Int}).toUShort())"
    | ``UInt16.decEq => return some s!"({← castIfNeeded a0 "UShort"} == {← castIfNeeded a1 "UShort"})"
    | ``UInt16.decLt => return some s!"({← castIfNeeded a0 "UShort"} < {← castIfNeeded a1 "UShort"})"
    | ``UInt16.decLe => return some s!"({← castIfNeeded a0 "UShort"} <= {← castIfNeeded a1 "UShort"})"

    -- Binary Float (Double)
    | `Float.add => return some s!"({← castIfNeeded a0 "Double"} + {← castIfNeeded a1 "Double"})"
    | `Float.sub => return some s!"({← castIfNeeded a0 "Double"} - {← castIfNeeded a1 "Double"})"
    | `Float.mul => return some s!"({← castIfNeeded a0 "Double"} * {← castIfNeeded a1 "Double"})"
    | `Float.div => return some s!"({← castIfNeeded a0 "Double"} / {← castIfNeeded a1 "Double"})"
    | `Float.beq => return some s!"({← castIfNeeded a0 "Double"} == {← castIfNeeded a1 "Double"})"
    | `Float.decLt | `Float.lt => return some s!"({← castIfNeeded a0 "Double"} < {← castIfNeeded a1 "Double"})"
    | `Float.decLe | `Float.le => return some s!"({← castIfNeeded a0 "Double"} <= {← castIfNeeded a1 "Double"})"
    | `Float.pow => return some s!"Math.pow({← castIfNeeded a0 "Double"}, {← castIfNeeded a1 "Double"})"
    | `Float.atan2 => return some s!"kotlin.math.atan2({← castIfNeeded a0 "Double"}, {← castIfNeeded a1 "Double"})"
    | `Float.scaleB => return some s!"Math.scalb({← castIfNeeded a0 "Double"}, {← castIfNeeded a1 "Int"})"
    | `Float.minimum => return some s!"kotlin.math.min({← castIfNeeded a0 "Double"}, {← castIfNeeded a1 "Double"})"
    | `Float.maximum => return some s!"kotlin.math.max({← castIfNeeded a0 "Double"}, {← castIfNeeded a1 "Double"})"
    | `Float.minimumNumber =>
      return some s!"(run \{ val x = {← castIfNeeded a0 "Double"}; val y = {← castIfNeeded a1 "Double"}; if (x.isNaN()) y else if (y.isNaN()) x else kotlin.math.min(x, y) })"
    | `Float.maximumNumber =>
      return some s!"(run \{ val x = {← castIfNeeded a0 "Double"}; val y = {← castIfNeeded a1 "Double"}; if (x.isNaN()) y else if (y.isNaN()) x else kotlin.math.max(x, y) })"

    -- Binary Float32 (Float)
    | `Float32.add => return some s!"({← castIfNeeded a0 "Float"} + {← castIfNeeded a1 "Float"})"
    | `Float32.sub => return some s!"({← castIfNeeded a0 "Float"} - {← castIfNeeded a1 "Float"})"
    | `Float32.mul => return some s!"({← castIfNeeded a0 "Float"} * {← castIfNeeded a1 "Float"})"
    | `Float32.div => return some s!"({← castIfNeeded a0 "Float"} / {← castIfNeeded a1 "Float"})"
    | `Float32.beq => return some s!"({← castIfNeeded a0 "Float"} == {← castIfNeeded a1 "Float"})"
    | `Float32.decLt | `Float32.lt => return some s!"({← castIfNeeded a0 "Float"} < {← castIfNeeded a1 "Float"})"
    | `Float32.decLe | `Float32.le => return some s!"({← castIfNeeded a0 "Float"} <= {← castIfNeeded a1 "Float"})"
    | `Float32.pow => return some s!"Math.pow(({← castIfNeeded a0 "Float"}).toDouble(), ({← castIfNeeded a1 "Float"}).toDouble()).toFloat()"
    | `Float32.atan2 => return some s!"kotlin.math.atan2({← castIfNeeded a0 "Float"}, {← castIfNeeded a1 "Float"})"
    | `Float32.scaleB => return some s!"Math.scalb({← castIfNeeded a0 "Float"}, {← castIfNeeded a1 "Int"})"
    | `Float32.minimum => return some s!"kotlin.math.min({← castIfNeeded a0 "Float"}, {← castIfNeeded a1 "Float"})"
    | `Float32.maximum => return some s!"kotlin.math.max({← castIfNeeded a0 "Float"}, {← castIfNeeded a1 "Float"})"
    | `Float32.minimumNumber =>
      return some s!"(run \{ val x = {← castIfNeeded a0 "Float"}; val y = {← castIfNeeded a1 "Float"}; if (x.isNaN()) y else if (y.isNaN()) x else kotlin.math.min(x, y) })"
    | `Float32.maximumNumber =>
      return some s!"(run \{ val x = {← castIfNeeded a0 "Float"}; val y = {← castIfNeeded a1 "Float"}; if (x.isNaN()) y else if (y.isNaN()) x else kotlin.math.max(x, y) })"

    -- Binary ByteArray & FloatArray
    | `ByteArray.push => return some s!"({← castIfNeeded a0 "ByteArray"} + ({← castIfNeeded a1 "UByte"}).toByte())"
    | `ByteArray.get! => return some s!"(({← castIfNeeded a0 "ByteArray"})[{← fromNatTo a1 "Int"}]).toUByte()"
    | `ByteArray.beq | `ByteArray.decEq => return some s!"({← castIfNeeded a0 "ByteArray"}).contentEquals({← castIfNeeded a1 "ByteArray"})"
    | `ByteSlice.beq =>
      return some s!"(run \{ val x = {a0} as Array<*>; val y = {a1} as Array<*>; val bx = x[1] as ByteArray; val sx = (x[2] as java.math.BigInteger).toInt().coerceIn(0, bx.size); val lx = (x[3] as java.math.BigInteger).toInt().coerceIn(0, bx.size - sx); val by = y[1] as ByteArray; val sy = (y[2] as java.math.BigInteger).toInt().coerceIn(0, by.size); val ly = (y[3] as java.math.BigInteger).toInt().coerceIn(0, by.size - sy); java.util.Arrays.equals(bx, sx, sx + lx, by, sy, sy + ly) })"
    | `ByteArray.propagateMark => return some a1
    | `ByteArray.fastAppend | `ByteArray.append => return some s!"({← castIfNeeded a0 "ByteArray"} + {← castIfNeeded a1 "ByteArray"})"
    | `FloatArray.push => return some s!"({← castIfNeeded a0 "DoubleArray"} + {← castIfNeeded a1 "Double"})"
    | `FloatArray.get! => return some s!"({← castIfNeeded a0 "DoubleArray"})[{← fromNatTo a1 "Int"}]"
    | `FloatArray.propagateMark => return some a1

    -- Binary String
    | `String.append | `String.Internal.append => return some s!"({← castIfNeeded a0 "String"} + {← castIfNeeded a1 "String"})"
    | `String.push => return some s!"({← castIfNeeded a0 "String"} + Character.toString(({a1}).toInt()))"
    | `String.decEq => return some s!"({← castIfNeeded a0 "String"} == {← castIfNeeded a1 "String"})"
    | `String.decLt | `String.decidableLT | `String.lt => return some s!"({← castIfNeeded a0 "String"} < {← castIfNeeded a1 "String"})"
    | `String.compare =>
      return some s!"(run \{ val c = ({← castIfNeeded a0 "String"}).compareTo({← castIfNeeded a1 "String"}); if (c < 0) 0 else if (c == 0) 1 else 2 })"
    | `String.Slice.instDecidableLt =>
      return some s!"(run \{ val x = {a0} as Array<*>; val y = {a1} as Array<*>; val sx = (x[1] as String).encodeToByteArray(); val bx = (x[2] as java.math.BigInteger).toInt().coerceIn(0, sx.size); val ex = (x[3] as java.math.BigInteger).toInt().coerceIn(bx, sx.size); val sy = (y[1] as String).encodeToByteArray(); val by = (y[2] as java.math.BigInteger).toInt().coerceIn(0, sy.size); val ey = (y[3] as java.math.BigInteger).toInt().coerceIn(by, sy.size); java.util.Arrays.compareUnsigned(sx, bx, ex, sy, by, ey) < 0 })"
    | `String.Internal.isPrefixOf => return some s!"({← castIfNeeded a1 "String"}).startsWith({← castIfNeeded a0 "String"})"
    | `String.propagateMark => return some a1
    | `String.ofByteArray | `String.fromUTF8 =>
      return some s!"({← castIfNeeded a0 "ByteArray"}).decodeToString()"
    | `String.getUTF8Byte | `String.Internal.getUTF8Byte | `String.Internal.ugetUTF8Byte =>
      return some s!"({← castIfNeeded a0 "String"}).encodeToByteArray()[{← fromNatTo a1 "Int"}].toUByte()"
    | `String.Pos.Raw.isValid =>
      return some s!"(run \{ val bs = ({← castIfNeeded a0 "String"}).encodeToByteArray(); val idx = {← fromNatTo a1 "Int"}; idx == bs.size || (idx in 0 until bs.size && (bs[idx].toInt() and 0xC0) != 0x80) })"
    | `String.Pos.Raw.atEnd | `String.atEnd | `String.Internal.atEnd =>
      return some s!"({← fromNatTo a1 "Int"} >= ({← castIfNeeded a0 "String"}).encodeToByteArray().size)"
    | `String.decodeChar | `String.Pos.Raw.get | `String.get | `String.Pos.Raw.get' | `String.get' | `String.Pos.Raw.get! | `String.get! | `String.Internal.get =>
      return some s!"(run \{ val bs = ({← castIfNeeded a0 "String"}).encodeToByteArray(); val idx = {← fromNatTo a1 "Int"}; if (idx < 0 || idx >= bs.size) 65u else \{ val b0 = bs[idx].toInt() and 0xFF; val len = if (b0 < 0x80) 1 else if (b0 < 0xC0) 0 else if (b0 < 0xE0) 2 else if (b0 < 0xF0) 3 else 4; if (len == 0 || idx + len > bs.size) 65u else bs.decodeToString(idx, idx + len).codePointAt(0).toUInt() } })"
    | `String.Pos.Raw.get? | `String.get? =>
      return some s!"(run \{ val bs = ({← castIfNeeded a0 "String"}).encodeToByteArray(); val idx = {← fromNatTo a1 "Int"}; if (idx < 0 || idx >= bs.size) arrayOf<Any?>(0) else \{ val b0 = bs[idx].toInt() and 0xFF; val len = if (b0 < 0x80) 1 else if (b0 < 0xC0) 0 else if (b0 < 0xE0) 2 else if (b0 < 0xF0) 3 else 4; if (len == 0 || idx + len > bs.size) arrayOf<Any?>(0) else arrayOf<Any?>(1, bs.decodeToString(idx, idx + len).codePointAt(0).toUInt()) } })"
    | `String.Pos.next | `String.Pos.Raw.next | `String.next | `String.Pos.Raw.next' | `String.next' | `String.Internal.next =>
      return some s!"java.math.BigInteger.valueOf((run \{ val bs = ({← castIfNeeded a0 "String"}).encodeToByteArray(); val idx = {← fromNatTo a1 "Int"}; if (idx < 0 || idx >= bs.size) idx + 1 else \{ val b0 = bs[idx].toInt() and 0xFF; idx + (if (b0 < 0xC0) 1 else if (b0 < 0xE0) 2 else if (b0 < 0xF0) 3 else 4) } }).toLong())"
    | `String.Pos.Raw.prev | `String.prev =>
      return some s!"java.math.BigInteger.valueOf((run \{ val bs = ({← castIfNeeded a0 "String"}).encodeToByteArray(); var idx = {← fromNatTo a1 "Int"} - 1; while (idx > 0 && idx < bs.size && (bs[idx].toInt() and 0xC0) == 0x80) \{ idx -= 1 }; maxOf(0, idx) }).toLong())"

    -- Binary Int (Int)
    | `Int.add => return some s!"({← castIfNeeded a0 "Int"} + {← castIfNeeded a1 "Int"})"
    | `Int.sub => return some s!"({← castIfNeeded a0 "Int"} - {← castIfNeeded a1 "Int"})"
    | `Int.mul => return some s!"({← castIfNeeded a0 "Int"} * {← castIfNeeded a1 "Int"})"
    | `Int.div | `Int.tdiv => return some s!"({← castIfNeeded a0 "Int"} / {← castIfNeeded a1 "Int"})"
    | `Int.mod | `Int.tmod => return some s!"({← castIfNeeded a0 "Int"} % {← castIfNeeded a1 "Int"})"
    | `Int.ediv =>
      let x ← castIfNeeded a0 "Int"
      let y ← castIfNeeded a1 "Int"
      return some s!"(if ({y} == 0) 0 else if ({y} > 0) Math.floorDiv({x}, {y}) else -Math.floorDiv({x}, -{y}))"
    | `Int.emod =>
      let x ← castIfNeeded a0 "Int"
      let y ← castIfNeeded a1 "Int"
      return some s!"(if ({y} == 0) {x} else Math.floorMod({x}, kotlin.math.abs({y})))"
    | `Int.decEq => return some s!"({← castIfNeeded a0 "Int"} == {← castIfNeeded a1 "Int"})"
    | `Int.decLt => return some s!"({← castIfNeeded a0 "Int"} < {← castIfNeeded a1 "Int"})"
    | `Int.decLe => return some s!"({← castIfNeeded a0 "Int"} <= {← castIfNeeded a1 "Int"})"

    | ``Bool.decEq => return some s!"({a0} == {a1})"
    | ``Nat.decEq | `Nat.beq =>
      if let (some i0, some i1) := (stripBigIntToInt? a0, stripBigIntToInt? a1) then
        return some s!"({i0} == {i1})"
      return some s!"({← castIfNeeded a0 "java.math.BigInteger"} == {← castIfNeeded a1 "java.math.BigInteger"})"
    | ``Nat.decLt | `Nat.blt =>
      if let (some i0, some i1) := (stripBigIntToInt? a0, stripBigIntToInt? a1) then
        return some s!"({i0} < {i1})"
      return some s!"({← castIfNeeded a0 "java.math.BigInteger"} < {← castIfNeeded a1 "java.math.BigInteger"})"
    | ``Nat.decLe | `Nat.ble =>
      if let (some i0, some i1) := (stripBigIntToInt? a0, stripBigIntToInt? a1) then
        return some s!"({i0} <= {i1})"
      return some s!"({← castIfNeeded a0 "java.math.BigInteger"} <= {← castIfNeeded a1 "java.math.BigInteger"})"
    | `Nat.land =>
      return some s!"({← castIfNeeded a0 "java.math.BigInteger"}.and({← castIfNeeded a1 "java.math.BigInteger"}))"
    | `Nat.lor =>
      return some s!"({← castIfNeeded a0 "java.math.BigInteger"}.or({← castIfNeeded a1 "java.math.BigInteger"}))"
    | `Nat.xor =>
      return some s!"({← castIfNeeded a0 "java.math.BigInteger"}.xor({← castIfNeeded a1 "java.math.BigInteger"}))"
    | `Nat.shiftLeft =>
      return some s!"({← castIfNeeded a0 "java.math.BigInteger"}.shiftLeft({← fromNatTo a1 "Int"}))"
    | `Nat.shiftRight =>
      return some s!"({← castIfNeeded a0 "java.math.BigInteger"}.shiftRight({← fromNatTo a1 "Int"}))"
    | `Nat.add =>
      return some s!"({← castIfNeeded a0 "java.math.BigInteger"}.add({← castIfNeeded a1 "java.math.BigInteger"}))"
    | `Nat.sub =>
      return some s!"({← castIfNeeded a0 "java.math.BigInteger"}.subtract({← castIfNeeded a1 "java.math.BigInteger"}).max(java.math.BigInteger.ZERO))"
    | `Nat.mul =>
      return some s!"({← castIfNeeded a0 "java.math.BigInteger"}.multiply({← castIfNeeded a1 "java.math.BigInteger"}))"
    | `Nat.div | `Nat.divExact =>
      let x ← castIfNeeded a0 "java.math.BigInteger"
      let y ← castIfNeeded a1 "java.math.BigInteger"
      return some s!"(if ({y} == java.math.BigInteger.ZERO) java.math.BigInteger.ZERO else ({x}).divide({y}))"
    | `Nat.mod | `Nat.modCore =>
      let x ← castIfNeeded a0 "java.math.BigInteger"
      let y ← castIfNeeded a1 "java.math.BigInteger"
      return some s!"(if ({y} == java.math.BigInteger.ZERO) {x} else ({x}).remainder({y}))"
    | `Nat.pow =>
      return some s!"({← castIfNeeded a0 "java.math.BigInteger"}.pow({← fromNatTo a1 "Int"}))"
    | `Nat.gcd =>
      return some s!"({← castIfNeeded a0 "java.math.BigInteger"}.gcd({← castIfNeeded a1 "java.math.BigInteger"}))"
    | _ => return none
  if args.size == 3 then
    let a0 ← toKotlinArg args[0]!
    let a1 ← toKotlinArg args[1]!
    let a2 ← toKotlinArg args[2]!
    match fn with
    | `Float.fma =>
      return some s!"Math.fma({← castIfNeeded a0 "Double"}, {← castIfNeeded a1 "Double"}, {← castIfNeeded a2 "Double"})"
    | `Float32.fma =>
      return some s!"Math.fma({← castIfNeeded a0 "Float"}, {← castIfNeeded a1 "Float"}, {← castIfNeeded a2 "Float"})"
    | `Float.ofScientific =>
      if let (some d0, some d2) := (extractNatLiteralDigits? a0, extractNatLiteralDigits? a2) then
        if a1 == "true" || a1 == "false" then
          if d2 == "0" then return some s!"{d0}.0"
          else if a1 == "true" then return some s!"{d0}e-{d2}"
          else return some s!"{d0}e{d2}"
      return some s!"(({← fromNatTo a0 "Double"}) * Math.pow(10.0, if ({a1}) -({← fromNatTo a2 "Double"}) else ({← fromNatTo a2 "Double"})))"
    | `Float32.ofScientific =>
      if let (some d0, some d2) := (extractNatLiteralDigits? a0, extractNatLiteralDigits? a2) then
        if a1 == "true" || a1 == "false" then
          if d2 == "0" then return some s!"{d0}.0f"
          else if a1 == "true" then return some s!"{d0}e-{d2}f"
          else return some s!"{d0}e{d2}f"
      return some s!"(({← fromNatTo a0 "Double"}) * Math.pow(10.0, if ({a1}) -({← fromNatTo a2 "Double"}) else ({← fromNatTo a2 "Double"}))).toFloat()"
    | `ByteArray.get | `ByteArray.uget =>
      return some s!"(({← castIfNeeded a0 "ByteArray"})[{← fromNatTo a1 "Int"}]).toUByte()"
    | `ByteArray.extract =>
      return some s!"(run \{ val bs = {← castIfNeeded a0 "ByteArray"}; val start = ({← fromNatTo a1 "Int"}).coerceIn(0, bs.size); val stop = ({← fromNatTo a2 "Int"}).coerceIn(start, bs.size); bs.copyOfRange(start, stop) })"
    | `FloatArray.get | `FloatArray.uget =>
      return some s!"({← castIfNeeded a0 "DoubleArray"})[{← fromNatTo a1 "Int"}]"
    | `String.extract | `String.Pos.extract | `String.Pos.Raw.extract | `String.Internal.extract =>
      return some s!"(run \{ val bs = ({← castIfNeeded a0 "String"}).encodeToByteArray(); val start = ({← fromNatTo a1 "Int"}).coerceIn(0, bs.size); val stop = ({← fromNatTo a2 "Int"}).coerceIn(start, bs.size); bs.decodeToString(start, stop) })"
    | `String.Pos.set | `String.Pos.Raw.set | `String.set =>
      return some s!"(run \{ val bs = ({← castIfNeeded a0 "String"}).encodeToByteArray(); val idx = {← fromNatTo a1 "Int"}; if (idx < 0 || idx >= bs.size) {← castIfNeeded a0 "String"} else \{ val b0 = bs[idx].toInt() and 0xFF; val len = if (b0 < 0x80) 1 else if (b0 < 0xC0) 0 else if (b0 < 0xE0) 2 else if (b0 < 0xF0) 3 else 4; if (len == 0 || idx + len > bs.size) {← castIfNeeded a0 "String"} else bs.decodeToString(0, idx) + Character.toString(({← castIfNeeded a2 "UInt"}).toInt()) + bs.decodeToString(idx + len, bs.size) } })"
    | _ => return none
  if args.size == 4 then
    let a0 ← toKotlinArg args[0]!
    let a1 ← toKotlinArg args[1]!
    let a2 ← toKotlinArg args[2]!
    let a3 ← toKotlinArg args[3]!
    if fn == `mkPanicMessage then
      return some s!"(\"PANIC at \" + {← castIfNeeded a0 "String"} + \":\" + ({a1}).toString() + \":\" + ({a2}).toString() + \": \" + {← castIfNeeded a3 "String"})"
  if args.size == 5 then
    let a0 ← toKotlinArg args[0]!
    let a1 ← toKotlinArg args[1]!
    let a2 ← toKotlinArg args[2]!
    let a3 ← toKotlinArg args[3]!
    let a4 ← toKotlinArg args[4]!
    if fn == `mkPanicMessageWithDecl then
      return some s!"(\"PANIC at \" + {← castIfNeeded a1 "String"} + \" \" + {← castIfNeeded a0 "String"} + \":\" + ({a2}).toString() + \":\" + ({a3}).toString() + \": \" + {← castIfNeeded a4 "String"})"
    if fn == `String.Slice.Pattern.Internal.memcmpStr then
      return some s!"java.util.Arrays.equals(({← castIfNeeded a0 "String"}).encodeToByteArray(), {← fromNatTo a2 "Int"}, {← fromNatTo a2 "Int"} + {← fromNatTo a4 "Int"}, ({← castIfNeeded a1 "String"}).encodeToByteArray(), {← fromNatTo a3 "Int"}, {← fromNatTo a3 "Int"} + {← fromNatTo a4 "Int"})"
  if args.size == 6 then
    if fn == `ByteArray.copySlice then
      let a0 ← toKotlinArg args[0]!
      let a1 ← toKotlinArg args[1]!
      let a2 ← toKotlinArg args[2]!
      let a3 ← toKotlinArg args[3]!
      let a4 ← toKotlinArg args[4]!
      return some s!"(run \{ val s = {← castIfNeeded a0 "ByteArray"}; val d = {← castIfNeeded a2 "ByteArray"}; val sOff = {← fromNatTo a1 "Int"}; if (sOff < 0 || sOff > s.size) d.copyOf() else \{ val l = minOf(maxOf(0, {← fromNatTo a4 "Int"}), s.size - sOff); val dOff = ({← fromNatTo a3 "Int"}).coerceIn(0, d.size); val r = d.copyOf(maxOf(d.size, dOff + l)); s.copyInto(r, dOff, sOff, sOff + l); r } })"
  return none

/--
Reference to field `name` of `recv`. On `this`, the field is referenced unqualified unless a
parameter of the current function has the same name.
-/
def fieldRef (recv name : String) : EmitM String := do
  let propName := if isKotlinKeyword name then s!"`{name}`" else name
  if recv != "this" then return s!"{recv}.{propName}"
  let shadowed ← (← read).currParams.anyM fun p => return (← getVarName p.fvarId) == name
  return if shadowed then s!"this.{propName}" else propName

/-- Kotlin element type of a primitive Kotlin array type; `none` for `Array<Any?>`. -/
def kotlinArrayElem? (arrTy : String) : Option String :=
  match arrTy with
  | "LongArray" => some "Long"
  | "IntArray" => some "Int"
  | "ShortArray" => some "Short"
  | "ByteArray" => some "Byte"
  | "ULongArray" => some "ULong"
  | "UIntArray" => some "UInt"
  | "UShortArray" => some "UShort"
  | "UByteArray" => some "UByte"
  | "BooleanArray" => some "Boolean"
  | "DoubleArray" => some "Double"
  | "FloatArray" => some "Float"
  | _ => none

/-- Kotlin type of a variable with a `kotlin:` type, e.g. an `Array` with `typedArrays`. -/
def kotlinTypeOf? (a : Arg .impure) : EmitM (Option String) := do
  let .fvar f := a | return none
  match getJvmTypeDesc? (← getType f) with
  | some d => if d.startsWith "kotlin:" then return some (d.drop 7).toString
  | none => pure ()
  let name ← getVarName f
  if let some t := (← get).varTypes[name]? then
    if t != "Any?" then return some t
  return none

def arrayType (a : Arg .impure) : EmitM String := do
  return (← kotlinTypeOf? a).getD "Array<Any?>"

def isZeroElem (elemTy vc : String) : Bool :=
  match elemTy with
  | "Long" => vc == "0L" || vc == "0"
  | "Int" => vc == "0"
  | "Short" => vc == "(0).toShort()" || vc == "0.toShort()"
  | "Byte" => vc == "(0).toByte()" || vc == "0.toByte()"
  | "ULong" => vc == "0uL"
  | "UInt" => vc == "0u"
  | "UShort" => vc == "(0u).toUShort()" || vc == "0u.toUShort()"
  | "UByte" => vc == "(0u).toUByte()" || vc == "0u.toUByte()"
  | "Boolean" => vc == "false"
  | "Double" => vc == "0.0"
  | "Float" => vc == "0.0f"
  | _ => false

/-- Kotlin allocation of an array of type `arrTy` with `n` elements equal to `v`. -/
def kotlinArrayAlloc (arrTy n v : String) : EmitM String := do
  match kotlinArrayElem? arrTy with
  | some e =>
    let vc ← castIfNeeded v e
    if isZeroElem e vc then
      return s!"{arrTy}({n})"
    else
      return s!"{arrTy}({n}).apply \{ fill({vc}) }"
  | none =>
    if v == "null" then
      return s!"arrayOfNulls<Any?>({n})"
    else
      return s!"arrayOfNulls<Any?>({n}).also \{ it.fill({v}) }"

def arrOp (s : String) : Name := .str `Array s

/-- Reads and allocations of `Array`, `ByteArray`, and `FloatArray`. -/
def emitArrayRead? (fn : Name) (args : Array (Arg .impure)) (resTy : Expr) : EmitM (Option String) := do
  let fn := match fn with | .str p "_boxed" => p | _ => fn
  let arg (i : Nat) : EmitM String := toKotlinArg (args[i]?.getD .erased)
  if fn == arrOp "get!Internal" || fn == arrOp "get!InternalBorrowed" then
    let arrTy ← arrayType (args[2]?.getD .erased)
    return some s!"{← castIfNeeded (← arg 2) arrTy}[{← fromNatTo (← arg 3) "Int"}]"
  if fn == arrOp "getInternal" || fn == arrOp "getInternalBorrowed" then
    let arrTy ← arrayType (args[1]?.getD .erased)
    return some s!"{← castIfNeeded (← arg 1) arrTy}[{← fromNatTo (← arg 2) "Int"}]"
  if fn == arrOp "uget" || fn == arrOp "ugetBorrowed" then
    let arrTy ← arrayType (args[1]?.getD .erased)
    return some s!"{← castIfNeeded (← arg 1) arrTy}[{← fromNatTo (← arg 2) "Int"}]"
  if fn == arrOp "size" then
    let arrTy ← arrayType (args[1]?.getD .erased)
    return some s!"java.math.BigInteger.valueOf({← castIfNeeded (← arg 1) arrTy}.size.toLong())"
  if fn == arrOp "usize" then
    let arrTy ← arrayType (args[1]?.getD .erased)
    return some s!"{← castIfNeeded (← arg 1) arrTy}.size"
  if fn == arrOp "replicate" || fn == arrOp "mkArray" then
    let arrTy := match getJvmTypeDesc? resTy with
      | some d => (d.drop 7).toString
      | none => "Array<Any?>"
    return some (← kotlinArrayAlloc arrTy (← fromNatTo (← arg 1) "Int") (← arg 2))
  if fn == arrOp "mkEmpty" || fn == arrOp "emptyWithCapacity" then
    let t := match getJvmTypeDesc? resTy with
      | some d => (d.drop 7).toString
      | none => "Array<Any?>"
    return some (if (kotlinArrayElem? t).isSome then s!"{t}(0)" else "arrayOfNulls<Any?>(0)")
  if fn == arrOp "toList" then
    let arrTy ← arrayType (args[1]?.getD .erased)
    let aStr ← castIfNeeded (← arg 1) arrTy
    return some s!"({aStr}).foldRight(arrayOf<Any?>(0) as Any?) \{ elem, acc -> arrayOf<Any?>(1, elem, acc) }"
  if fn == arrOp "mk" || fn == ``List.toArray || fn == `List.toArrayImpl || fn == `List.toArrayImpl._redArg then
    let arrTy := match getJvmTypeDesc? resTy with
      | some d => (d.drop 7).toString
      | none => "Array<Any?>"
    let listStr ← arg (args.size - 1)
    match kotlinArrayElem? arrTy with
    | some e =>
      return some s!"(run \{ val tmp = ArrayList<{e}>(); var cur: Any? = {listStr}; while (((cur as Array<*>)[0] as Int) == 1) \{ tmp.add((cur as Array<*>)[1] as {e}); cur = (cur as Array<*>)[2] }; {arrTy}(tmp.size) \{ tmp[it] } })"
    | none =>
      return some s!"(run \{ val tmp = ArrayList<Any?>(); var cur: Any? = {listStr}; while (((cur as Array<*>)[0] as Int) == 1) \{ tmp.add((cur as Array<*>)[1]); cur = (cur as Array<*>)[2] }; tmp.toTypedArray() })"
  if fn == `ByteArray.mk then
    let arrTy ← arrayType (args[0]?.getD .erased)
    let a0 ← arg 0
    if arrTy == "UByteArray" then
      let aStr ← castIfNeeded a0 "UByteArray"
      return some s!"ByteArray({aStr}.size) \{ ({aStr}[it]).toByte() }"
    else
      let aStr ← castIfNeeded a0 "Array<Any?>"
      return some s!"ByteArray({aStr}.size) \{ ({aStr}[it] as UByte).toByte() }"
  if fn == `ByteArray.data then
    let resArrTy := match getJvmTypeDesc? resTy with | some d => (d.drop 7).toString | none => "UByteArray"
    let aStr ← castIfNeeded (← arg 0) "ByteArray"
    if resArrTy == "UByteArray" then
      return some s!"UByteArray({aStr}.size) \{ ({aStr}[it]).toUByte() }"
    else
      return some s!"Array<Any?>({aStr}.size) \{ ({aStr}[it]).toUByte() }"
  if fn == `FloatArray.mk then
    let arrTy ← arrayType (args[0]?.getD .erased)
    let a0 ← arg 0
    if arrTy == "DoubleArray" then
      let aStr ← castIfNeeded a0 "DoubleArray"
      return some s!"({aStr}).copyOf()"
    else
      let aStr ← castIfNeeded a0 "Array<Any?>"
      return some s!"DoubleArray({aStr}.size) \{ ({aStr}[it] as Double) }"
  if fn == `FloatArray.data then
    let resArrTy := match getJvmTypeDesc? resTy with | some d => (d.drop 7).toString | none => "DoubleArray"
    let aStr ← castIfNeeded (← arg 0) "DoubleArray"
    if resArrTy == "DoubleArray" then
      return some s!"({aStr}).copyOf()"
    else
      return some s!"Array<Any?>({aStr}.size) \{ {aStr}[it] }"
  if fn == arrOp "push" then
    let arrTy ← match getJvmTypeDesc? resTy with
      | some d => pure (d.drop 7).toString
      | none => arrayType (args[1]?.getD .erased)
    let aStr ← castIfNeeded (← arg 1) arrTy
    let vStr ← arg 2
    match kotlinArrayElem? arrTy with
    | some e =>
      let vc ← castIfNeeded vStr e
      return some s!"({aStr} + {vc})"
    | none =>
      return some s!"({aStr}.copyOf({aStr}.size + 1).also \{ it[{aStr}.size] = {vStr} })"
  if fn == arrOp "pop" then
    let arrTy ← arrayType (args[1]?.getD .erased)
    let aStr ← castIfNeeded (← arg 1) arrTy
    return some s!"(if ({aStr}.isNotEmpty()) {aStr}.copyOf({aStr}.size - 1) else {aStr})"
  if fn == arrOp "append" || fn == `Array.append._redArg ||
     fn == arrOp "appendCore" || fn == `Array.appendCore._redArg then
    let aIdx := args.size - 2
    let bIdx := args.size - 1
    let arrTy ← arrayType (args[aIdx]?.getD .erased)
    let aStr ← castIfNeeded (← arg aIdx) arrTy
    let bStr ← castIfNeeded (← arg bIdx) arrTy
    return some s!"({aStr} + {bStr})"
  if fn == arrOp "extract" || fn == `Array.extract._redArg then
    let aIdx := args.size - 3
    let startIdx := args.size - 2
    let stopIdx := args.size - 1
    let arrTy ← arrayType (args[aIdx]?.getD .erased)
    let aStr ← castIfNeeded (← arg aIdx) arrTy
    let startStr ← fromNatTo (← arg startIdx) "Int"
    let stopStr ← fromNatTo (← arg stopIdx) "Int"
    return some s!"{aStr}.copyOfRange(({startStr}).coerceIn(0, {aStr}.size), ({stopStr}).coerceIn(({startStr}).coerceIn(0, {aStr}.size), {aStr}.size))"
  if (fn == arrOp "propagateMark" || fn == `Array.propagateMark._redArg) && !args.isEmpty then
    return some (← arg (args.size - 1))
  if (fn == arrOp "markLinear" || fn == `Array.markLinear._redArg) && !args.isEmpty then
    let aIdx := args.size - 1
    let arrTy ← arrayType (args[aIdx]?.getD .erased)
    let aStr ← castIfNeeded (← arg aIdx) arrTy
    return some s!"({aStr}).copyOf()"
  return none

/-- In-place `Array`/`ByteArray`/`FloatArray` updates: `(array arg, index expr, value arg, checkBounds)`. -/
def arraySet? (fn : Name) (args : Array (Arg .impure)) : EmitM (Option (Arg .impure × String × Arg .impure × Bool)) := do
  let fn := match fn with | .str p "_boxed" => p | _ => fn
  if fn == arrOp "set!" || fn == arrOp "setIfInBounds" then
    return some (args[1]!, ← fromNatTo (← toKotlinArg args[2]!) "Int", args[3]!, false)
  if fn == arrOp "uset" || fn == arrOp "fset" || fn == arrOp "set" then
    return some (args[1]!, ← fromNatTo (← toKotlinArg args[2]!) "Int", args[3]!, false)
  if fn == `ByteArray.set! || fn == `FloatArray.set! then
    return some (args[0]!, ← fromNatTo (← toKotlinArg args[1]!) "Int", args[2]!, true)
  if fn == `ByteArray.set || fn == `ByteArray.uset ||
     fn == `FloatArray.set || fn == `FloatArray.uset then
    return some (args[0]!, ← fromNatTo (← toKotlinArg args[1]!) "Int", args[2]!, false)
  return none

/-- In-place `Array` element swaps: `(array arg, idx1 expr, idx2 expr, checkBounds)`. -/
def arraySwap? (fn : Name) (args : Array (Arg .impure)) : EmitM (Option (Arg .impure × String × String × Bool)) := do
  let fn := match fn with | .str p "_boxed" => p | _ => fn
  if fn == arrOp "swap" || fn == arrOp "uswap" then
    let iStr ← fromNatTo (← toKotlinArg args[2]!) "Int"
    let jStr ← fromNatTo (← toKotlinArg args[3]!) "Int"
    return some (args[1]!, iStr, jStr, false)
  if fn == arrOp "swapIfInBounds" then
    let iStr ← fromNatTo (← toKotlinArg args[2]!) "Int"
    let jStr ← fromNatTo (← toKotlinArg args[3]!) "Int"
    return some (args[1]!, iStr, jStr, true)
  return none

def toKotlinTypeParamName (n : Name) (idx : Nat) : String :=
  let s := n.eraseMacroScopes.toString
  match s with
  | "α" => if idx == 0 then "T" else "A"
  | "β" => if idx == 1 then "U" else "B"
  | "γ" => if idx == 2 then "V" else "C"
  | "κ" => "K"
  | "υ" => "V"
  | "ρ" => "R"
  | other =>
    if other.isEmpty || other == "_" then s!"T{idx + 1}"
    else if other.length == 1 then s!"{other.front.toUpper}"
    else toPascalCase other

def getDeclTypeParams (env : Environment) (declName : Name) : MetaM (Array (Name × String)) := do
  let some info := env.find? declName | return #[]
  Meta.forallTelescopeReducing info.type fun xs _ => do
    let mut typeParams := #[]
    let mut idx := 0
    for x in xs do
      let type ← Meta.inferType x
      if type.isSort then
        let binderName := (← x.fvarId!.getUserName).eraseMacroScopes
        let ktName := toKotlinTypeParamName binderName idx
        typeParams := typeParams.push (binderName, ktName)
        idx := idx + 1
    return typeParams

def getDeclNumTypeParams (env : Environment) (declName : Name) : MetaM Nat := do
  let some info := env.find? declName | return 0
  Meta.forallTelescopeReducing info.type fun xs _ => do
    let mut count := 0
    for x in xs do
      let type ← Meta.inferType x
      if type.isSort then count := count + 1
      else break
    return count

def isCompilerAuxDecl (env : Environment) (n : Name) : Bool :=
  isAuxRecursor env n || isNoConfusion env n || isRecCore env n ||
  match n with
  | .str _ s =>
    s.endsWith "_redArg" || s.contains "ctorIdx" || s.contains "ctorElim" ||
    s.endsWith "_elim" || s.startsWith "_cstage" || s.startsWith "_jp"
  | _ => false

partial def formatGenericKotlinType (env : Environment) (typeParams : Std.HashMap Name String) (type : Expr) : MetaM String := do
  let type ← Meta.whnf type
  match (type : Expr) with
  | .fvar fvarId =>
    let uName := (← fvarId.getUserName).eraseMacroScopes
    if let some t := typeParams[uName]? then return t
    return "Any?"
  | .bvar .. => return "Any?"
  | .const declName _ =>
    if let some t := typeParams[declName]? then return t
    if declName == ``Bool || declName == ``Decidable then return "Boolean"
    if declName == ``Nat then return "java.math.BigInteger"
    if declName == ``String then return "String"
    if declName == ``Unit || declName == ``PUnit then return "Unit"
    if declName == ``UInt8 then return "UByte"
    if declName == ``UInt16 then return "UShort"
    if declName == ``UInt32 then return "UInt"
    if declName == ``UInt64 then return "ULong"
    if declName == ``USize then return "Int"
    if declName == ``Int8 then return "Byte"
    if declName == ``Int16 then return "Short"
    if declName == ``Int32 then return "Int"
    if declName == ``Int64 then return "Long"
    if declName == ``ISize then return "Int"
    if declName == ``Float then return "Double"
    if declName == ``Float32 then return "Float"
    if declName == ``ByteArray then return "ByteArray"
    if declName == ``FloatArray then return "DoubleArray"
    if let some spec := Compiler.getKotlinClassSpec? env declName then
      if (← hasTrivialImpureStructure? declName).isSome then return "Any?"
      return spec.baseClassName
    return "Any?"
  | .app .. =>
    let fn := type.getAppFn
    let args := type.getAppArgs
    if fn.isConstOf ``Prod && args.size == 2 then
      let t0 ← formatGenericKotlinType env typeParams args[0]!
      let t1 ← formatGenericKotlinType env typeParams args[1]!
      return s!"Pair<{t0}, {t1}>"
    if fn.isConstOf ``Array && args.size == 1 then
      if compiler.kotlin.typedArrays.get (← getOptions) then
        let kt := kotlinArrayType args[0]!
        if kt != "Array<Any?>" then return kt
      return "Array<Any?>"
    if let .const declName _ := fn then
      if let some spec := Compiler.getKotlinClassSpec? env declName then
        if (← hasTrivialImpureStructure? declName).isSome then return "Any?"
        let base := spec.baseClassName
        let argTys ← args.mapM (formatGenericKotlinType env typeParams)
        let relevantArgs := argTys.filter (!·.isEmpty)
        if relevantArgs.isEmpty then return base
        else return s!"{base}<{String.intercalate ", " relevantArgs.toList}>"
    return "Any?"
  | .forallE .. =>
    Meta.forallTelescopeReducing type fun xs ret => do
      let paramTys ← xs.mapM fun x => do formatGenericKotlinType env typeParams (← Meta.inferType x)
      let retTy ← formatGenericKotlinType env typeParams ret
      let paramStr := String.intercalate ", " paramTys.toList
      return s!"({paramStr}) -> {retTy}"
  | _ => return "Any?"

/-- Kotlin property names and types of a `@[kotlin_class]` structure by runtime position. -/
structure ClassLayout where
  objs : Std.HashMap Nat String := {}
  objTypes : Std.HashMap Nat String := {}
  /-- By byte offset. -/
  scalars : Std.HashMap Nat String := {}
  usizes : Std.HashMap Nat String := {}
  orderedFields : Array (String × String) := #[]
  typeParams : Array String := #[]

def ctorClassLayout (ctorName : Name) : EmitM ClassLayout := do
  let env ← getEnv
  let some (.ctorInfo ctorVal) := env.find? ctorName | return {}
  let layout ← getCtorLayout ctorName
  let (typeParamNames, fieldNames, fieldKotlinTypes) ← (Meta.MetaM.run' do
    let typeParams ← getDeclTypeParams env ctorVal.induct
    let typeParamMap := typeParams.foldl (init := ({} : Std.HashMap Name String)) fun m (n, t) => m.insert n t
    Meta.forallTelescopeReducing ctorVal.type fun xs _ => do
      let fieldXs := xs.extract ctorVal.numParams xs.size
      let names ← fieldXs.mapM fun x => do
        let n := (← x.fvarId!.getUserName).eraseMacroScopes.toString
        return if n.isEmpty || n == "_" then "field" else n
      let types ← fieldXs.mapM fun x => do
        let fieldType ← Meta.inferType x
        let genTy ← formatGenericKotlinType env typeParamMap fieldType
        if genTy != "Any?" then return genTy
        let t ← toLCNFType fieldType
        let mt ← toMonoType t
        if isRichMonoType mt then return toKotlinType mt
        let it ← toImpureType mt
        return toKotlinType it
      return (typeParams.map (·.2), names, types) : CoreM (Array String × Array String × Array String))
  let mut r : ClassLayout := { typeParams := typeParamNames }
  let mut seenFieldNames : Std.HashSet String := {}
  for h : k in [:layout.fieldInfo.size] do
    let rawName := fieldNames[k]?.getD s!"field{k}"
    let baseName :=
      if isKotlinKeyword rawName then s!"`{rawName}`"
      else rawName
    let mut name := baseName
    if seenFieldNames.contains name then
      name := s!"{baseName}_{k}"
    seenFieldNames := seenFieldNames.insert name
    let kotlinTy := fieldKotlinTypes[k]?.getD "Any?"
    match layout.fieldInfo[k] with
    | .object i _ =>
      r := { r with
        objs := r.objs.insert i name
        objTypes := r.objTypes.insert i kotlinTy
        orderedFields := r.orderedFields.push (name, kotlinTy)
      }
    | .scalar _ off _ =>
      r := { r with
        scalars := r.scalars.insert off name
        orderedFields := r.orderedFields.push (name, kotlinTy)
      }
    | .usize i =>
      r := { r with
        usizes := r.usizes.insert i name
        orderedFields := r.orderedFields.push (name, kotlinTy)
      }
    | _ => pure ()
  return r

def classLayout (s : Name) : EmitM ClassLayout := do
  let env ← getEnv
  if env.find? s matches some (.ctorInfo _) then
    return ← ctorClassLayout s
  unless isStructure env s do
    return {}
  let fields := getStructureFields env s
  let ctorVal := getStructureCtor env s
  let layout ← getCtorLayout ctorVal.name
  let (typeParamNames, fieldKotlinTypes) ← (Meta.MetaM.run' do
    let typeParams ← getDeclTypeParams env s
    let typeParamMap := typeParams.foldl (init := ({} : Std.HashMap Name String)) fun m (n, t) => m.insert n t
    Meta.forallTelescopeReducing ctorVal.type fun xs _ => do
      let fieldXs := xs.extract ctorVal.numParams xs.size
      let fieldTys ← fieldXs.mapM fun x => do
        let fieldType ← Meta.inferType x
        let genTy ← formatGenericKotlinType env typeParamMap fieldType
        if genTy != "Any?" then return genTy
        let t ← toLCNFType fieldType
        let mt ← toMonoType t
        if isRichMonoType mt then return toKotlinType mt
        let it ← toImpureType mt
        return toKotlinType it
      return (typeParams.map (·.2), fieldTys) : CoreM (Array String × Array String))
  let tyOverrides := (Compiler.getKotlinTypes? env s).getD #[]
  let mut r : ClassLayout := { typeParams := typeParamNames }
  let mut relIdx := 0
  for h : k in [:layout.fieldInfo.size] do
    let name := match fields[k]? with
      | some (.str _ n) => n
      | _ => s!"field{k}"
    let defaultKotlinTy : String :=
      match layout.fieldInfo[k] with
      | .object _ _ => fieldKotlinTypes[k]?.getD "Any?"
      | .scalar _ _ irTy => toKotlinType irTy
      | .usize _ => "Int"
      | _ => "Any?"
    let kotlinTy := match tyOverrides[relIdx]? with
      | some t => if t == "_" then defaultKotlinTy else t
      | none => defaultKotlinTy
    match layout.fieldInfo[k] with
    | .object i _ =>
      r := { r with
        objs := r.objs.insert i name
        objTypes := r.objTypes.insert i kotlinTy
        orderedFields := r.orderedFields.push (name, kotlinTy)
      }
      relIdx := relIdx + 1
    | .scalar _ off _ =>
      r := { r with
        scalars := r.scalars.insert off name
        orderedFields := r.orderedFields.push (name, kotlinTy)
      }
      relIdx := relIdx + 1
    | .usize i =>
      r := { r with
        usizes := r.usizes.insert i name
        orderedFields := r.orderedFields.push (name, kotlinTy)
      }
      relIdx := relIdx + 1
    | _ => pure ()
  return r

/-- The `@[kotlin_class]` or `@[mutable_kotlin_class]` structure of variable `x`, from its Kotlin type. -/
def classStructOf? (x : FVarId) : EmitM (Option Name) := do
  let n ← getVarName x
  if let some s := (← get).nameStructs[n]? then
    return some s
  if let some t ← kotlinTypeOf? (.fvar x) then
    if let some s := (← read).classStructs[t]? then
      let env ← getEnv
      if isStructure env s then
        modify fun st => { st with nameStructs := st.nameStructs.insert n s }
        return some s
  return (← get).nameStructs[n]?

def mutableClassStructOf? (x : FVarId) : EmitM (Option Name) := do
  let some s ← classStructOf? x | return none
  return if Compiler.isMutableKotlinClass (← getEnv) s then some s else none

def isMutableClassFVar (x : FVarId) : EmitM Bool := do
  if let some t ← kotlinTypeOf? (.fvar x) then
    if let some s := (← read).classStructs[t]? then
      return Compiler.isMutableKotlinClass (← getEnv) s
  return (← mutableClassStructOf? x).isSome

/-- Kotlin property of runtime field `pos` of `x`, which must be a `@[kotlin_class]` value. -/
def classField (x : FVarId) (pos : ClassLayout → Option String) : EmitM String := do
  let some s ← classStructOf? x
    | throwError "Kotlin backend: field access on a value that is not a `@[kotlin_class]` structure"
  let some f := pos (← classLayout s)
    | throwError "Kotlin backend: unknown field of `{s}`"
  fieldRef (← getVarName x) f

/--
The result shape of `fn` if results identical to parameters are dropped: the result itself is a
parameter, or some components of a `Prod.mk` result are.
-/
def dropShape? (fn : Name) : EmitM (Option Ownership.RetShape) := do
  let declMap := (← read).declMap
  if let some d := declMap[fn]? then
    if d.type.isScalar then return none
  let some s := (← read).summaries[fn]? | return none
  let canDrop (j : Nat) : Bool := s.exclusive[j]?.getD false
  match s.ret with
  | .param j => return if canDrop j then some (.param j) else none
  | .prod comps =>
    let comps' := comps.map fun c? => c?.filter canDrop
    return if comps'.any (·.isSome) then some (.prod comps') else none
  | _ => return none

/-- Indices of the components that are still returned under a drop shape. -/
def keptComps : Ownership.RetShape → Array Nat
  | .prod comps => Id.run do
    let mut r := #[]
    for h : i in [:comps.size] do
      if comps[i].isNone then r := r.push i
    return r
  | _ => #[]

def isUnitShape : Ownership.RetShape → Bool
  | .param _ => true
  | s@(.prod _) => (keptComps s).isEmpty
  | _ => false

/-- Kotlin type of component `c` of a `Prod` type (without loose bound variables). -/
def prodComponentKotlinType? (type : Expr) (c : Nat) : EmitM (Option String) := do
  let type := type.consumeMData
  unless type.isAppOfArity ``Prod 2 do return none
  let comp := type.getAppArgs[c]!
  if comp.hasLooseBVars then return none
  try
    let t ← (Meta.MetaM.run' do
      let t ← toLCNFType comp
      let mono ← toMonoType t
      if isRichMonoType mono then return toKotlinType mono
      let impure ← toImpureType mono
      return toKotlinType impure : CoreM String)
    return if t == "Any?" then none else some t
  catch _ => return none

def monoResultType? (md : Decl .pure) : Option Expr :=
  let rec go (e : Expr) (n : Nat) : Option Expr :=
    match n with
    | 0 => some e
    | n + 1 =>
      match e with
      | .forallE _ _ b _ => go b n
      | _ => none
  go md.type md.params.size

def unboxedPapFn (fn : Name) : EmitM Name := do
  match fn with
  | .str p "_boxed" =>
    if (← read).declMap.contains p || (← getMonoDecl? p).isSome then return p
    else return fn
  | _ => return fn

/-- The Kotlin return type of declaration `fn`. -/
def fnRetKotlinType (fn : Name) (defaultTy : Expr) : EmitM String := do
  let env ← getEnv
  let rec resultType : Expr → Expr
    | .forallE _ _ b _ => resultType b
    | e => e
  let member? := Compiler.getKotlinMemberInfo? env fn
  let tyOverrides := (Compiler.getKotlinTypes? env fn).getD #[]
  if let some d := (← read).declMap[fn]? then
    let numEmitted := if member?.isSome then d.params.size - 1 else d.params.size
    if let some t := tyOverrides[numEmitted]? then
      if t != "_" then return t
  if let some shape ← dropShape? fn then
    if isUnitShape shape then return "Unit"
    let c := (keptComps shape)[0]!
    if let some ci := env.find? fn then
      if let some t ← prodComponentKotlinType? (resultType ci.type) c then
        return t
  let baseFn := match fn with | .str p "_boxed" => p | _ => fn
  if let some ci := (env.find? fn).orElse (fun _ => env.find? baseFn) then
    if resultType ci.type == mkConst ``Unit then return "Unit"
    if resultType ci.type == mkConst ``Nat then return "java.math.BigInteger"
    if resultType ci.type == mkConst ``String then return "String"
    if resultType ci.type == mkConst ``ByteArray then return "ByteArray"
    if resultType ci.type == mkConst ``FloatArray then return "DoubleArray"
  if let some d := (← read).declMap[fn]? then
    let t := toKotlinType d.type
    if t != "Any?" then return t
  if let some md ← getMonoDecl? fn then
    if let some r := monoResultType? md then
      if isRichMonoType r then return toKotlinType r
  return toKotlinType defaultTy

/--
Instantiates a `kotlin_expr:` template: `{i}` is replaced by the `i`-th argument; other characters,
including other braces, are copied.
-/
def instantiateKotlinTemplate (tmpl : String) (args : Array String) : String := Id.run do
  let cs := tmpl.toList.toArray
  let mut out := ""
  let mut i := 0
  while i < cs.size do
    let c := cs[i]!
    if c == '{' then
      let mut j := i + 1
      let mut n := 0
      while j < cs.size && cs[j]!.isDigit do
        n := n * 10 + (cs[j]!.toNat - '0'.toNat)
        j := j + 1
      if j > i + 1 && j < cs.size && cs[j]! == '}' then
        out := out ++ args[n]!
        i := j + 1
        continue
    out := out.push c
    i := i + 1
  return out

/--
Calls to `@[extern]` declarations with a Kotlin rendering:
* `kotlin_expr:<template>`: the template with `{i}` replaced by the `i`-th argument, e.g.
  `"kotlin_expr:({0}?.hashCode() ?: 0)"`.
* `kotlin_inplace:<template>`: a statement that updates the first argument in place, which is the
  result (e.g. `"kotlin_inplace:{0}.fill({1})"`); see `Ownership.isInplaceExtern`.
* `kotlin_op:cast`: unchecked cast to the declared result type, e.g. `Any?` to a type parameter.
* `kotlin_op:id`: the argument itself (a conversion that needs no Kotlin code).
* `kotlin_op:invoke`: calls the first argument, a value of Kotlin function type.
* `kotlin_op:is:<T>`: `(a is T)`.
* `kotlin_op:get:<field>` / `kotlin_op:set:<field>`: reads / writes a property of the first
  argument (unqualified on `this` unless shadowed by a parameter).
-/
def emitExternCall? (fn : Name) (args : Array (Arg .impure)) (resTy : Expr) : EmitM (Option String) := do
  if fn.isStr && (fn.getString!.startsWith "instInhabited" || fn.getString!.contains "inhabited") then
    return some (defaultKotlinVal (toKotlinType resTy))
  let env ← getEnv
  let some extStr := getExternNameFor env `kotlin fn | return none
  let runtimeArgs ← (Meta.MetaM.run' do
    let some info := env.find? fn | return args
    Meta.forallTelescopeReducing info.type fun xs _ => do
      let mut filtered : Array (Arg .impure) := #[]
      for i in [:min xs.size args.size] do
        let x := xs[i]!
        let ty ← Meta.inferType x
        if ty.isSort then continue
        if (← Meta.isClass? ty).isSome then continue
        filtered := filtered.push args[i]!
      if args.size > xs.size then
        for i in [xs.size:args.size] do
          filtered := filtered.push args[i]!
      return filtered : CoreM (Array (Arg .impure)))
  if extStr.startsWith "kotlin_expr:" then
    let argStrs ← runtimeArgs.mapM toKotlinArg
    return some (instantiateKotlinTemplate (extStr.drop 12).toString argStrs)
  if extStr.startsWith "kotlin_inplace:" then
    let argStrs ← runtimeArgs.mapM toKotlinArg
    return some (instantiateKotlinTemplate (extStr.drop 15).toString argStrs)
  unless extStr.startsWith "kotlin_op:" do return none
  let op := (extStr.drop 10).toString
  let argStrs ← runtimeArgs.mapM toKotlinArg
  match op with
  | "cast" => return some (← castIfNeeded argStrs[0]! (toKotlinType resTy))
  | "id" => return some argStrs[0]!
  | "invoke" =>
    let rest := (argStrs.extract 1 argStrs.size).toList
    return some s!"{argStrs[0]!}({String.intercalate ", " rest})"
  | _ =>
    if op.startsWith "is:" then
      return some s!"({argStrs[0]!} is {(op.drop 3).toString})"
    if op.startsWith "get:" then
      return some (← fieldRef argStrs[0]! (op.drop 4).toString)
    if op.startsWith "set:" then
      let lhs ← fieldRef argStrs[0]! (op.drop 4).toString
      return some s!"run \{ {lhs} = {argStrs[1]!}; Unit }"
    return none

def memberKotlinName (declName : Name) (info : Compiler.KotlinMemberInfo) : String :=
  info.name?.getD (match declName with | .str _ s => s | _ => toKotlinFnName declName)

/-- Calls to `@[kotlin_member]` declarations: `recv.name(rest)`, or `name(rest)` on `this`. -/
def emitMemberCall? (fn : Name) (args : Array (Arg .impure)) : EmitM (Option String) := do
  let some info := Compiler.getKotlinMemberInfo? (← getEnv) fn | return none
  if args.isEmpty then return none
  let numTypeParams ← (Meta.MetaM.run' (getDeclNumTypeParams (← getEnv) fn) : CoreM Nat)
  let runtimeArgs := args.extract numTypeParams args.size
  if runtimeArgs.isEmpty then return none
  let recv ← toKotlinArg runtimeArgs[0]!
  let tyOverrides := (Compiler.getKotlinTypes? (← getEnv) fn).getD #[]
  let rest ← (runtimeArgs.extract 1 runtimeArgs.size).mapIdxM fun i a => do
    let s ← toKotlinArg a
    match tyOverrides[i]? with
    | some t => if t == "_" then pure s else castIfNeeded s t
    | none => pure s
  let call := s!"{memberKotlinName fn info}({String.intercalate ", " rest.toList})"
  if recv == "this" then
    if (← read).currClass? == some info.className then
      return some call
    return some s!"{← castIfNeeded "this" info.recvType}.{call}"
  let sameType ← match runtimeArgs[0]! with
    | .fvar f => do pure (toKotlinType (← getType f) == info.recvType || (← get).varTypes[recv]? == some info.recvType)
    | .erased => pure false
  if sameType then
    return some s!"{recv}.{call}"
  return some s!"{← castIfNeeded recv info.recvType}.{call}"

def unsupportedProj (x : FVarId) : EmitM String := do
  throwError "Kotlin backend: in `{(← read).currFn}`: projection of `{← getVarName x}`, which is \
    not a `@[kotlin_class]` structure, is not supported"

def isInlinedConstLet? (d0 : LetDecl .impure) : Bool :=
  match d0.value with
  | .lit _ | .erased => true
  | .fap fn _ => fn.isStr && (fn.getString!.startsWith "instInhabited" || fn.getString!.contains "inhabited")
  | _ => false

/-- Returns the underlying literal `LetDecl` if `d` is a 0-argument constant wrapping a literal or inhabited default. -/
def isInlinedConstDecl? (d : Decl .impure) : Option (LetDecl .impure) :=
  if !d.params.isEmpty then none
  else match d.value with
  | .code (.let d0 (.return r)) =>
    if r == d0.fvarId && isInlinedConstLet? d0 then some d0 else none
  | .code (.let d0 (.let d1 (.return r))) =>
    if r == d1.fvarId && isInlinedConstLet? d0 && d1.value matches .box _ _ then some d0 else none
  | _ => none

def escapeKotlinString (s : String) : String :=
  s.foldl (init := "") fun acc c =>
    match c with
    | '\\' => acc ++ "\\\\"
    | '"' => acc ++ "\\\""
    | '$' => acc ++ "\\$"
    | '\n' => acc ++ "\\n"
    | '\r' => acc ++ "\\r"
    | '\t' => acc ++ "\\t"
    | _ => acc.push c

def formatAdtCtor (info : CtorInfo) (objs : Array String) (usizes : Std.HashMap Nat String)
    (scalars : Std.HashMap Nat String) : EmitM String := do
  let kb := (← get).knownBools
  let resolveBool (s : String) : String :=
    match kb[s]? with
    | some true => "true"
    | some false => "false"
    | none => s
  let mut elems : Array String := #[s!"{info.cidx}"]
  for i in [:info.size] do
    elems := elems.push (resolveBool (objs[i]?.getD "null"))
  for i in [info.size : info.size + info.usize] do
    elems := elems.push (usizes[i]?.getD "0")
  if !scalars.isEmpty then
    let maxOff := scalars.fold (init := 0) fun m k _ => max m k
    for off in [:maxOff + 1] do
      let v := match scalars[off]? with
        | some s => resolveBool s
        | none => "null"
      elems := elems.push v
  return s!"arrayOf<Any?>({String.intercalate ", " elems.toList})"

partial def emitLetValue (decl : LetDecl .impure) : EmitM String := do
  match decl.value with
  | .lit v =>
    match v with
    | .uint8 n =>
      if decl.type == ImpureType.bool then return if n == 0 then "false" else "true"
      else if decl.type == ImpureType.int8 then
        let s : Int := if n.toNat > 127 then (n.toNat : Int) - 256 else (n.toNat : Int)
        return s!"({s}).toByte()"
      else return s!"({n}u).toUByte()"
    | .uint16 n =>
      if decl.type == ImpureType.int16 then
        let s : Int := if n.toNat > 32767 then (n.toNat : Int) - 65536 else (n.toNat : Int)
        return s!"({s}).toShort()"
      else return s!"({n}u).toUShort()"
    | .uint32 n =>
      if decl.type == ImpureType.int32 then return formatInt32 n
      else return formatUInt32 n
    | .usize n => return formatInt32 n.toUInt32
    | .uint64 n =>
      if decl.type == ImpureType.int64 then return formatInt64 n
      else return formatUInt64 n
    | .nat n => return formatNatLit n
    | .str s => return s!"\"{escapeKotlinString s}\""
  | .erased => return "null"
  | .fap fn args =>
    -- Constants whose value is a literal (e.g. boxed literals) are inlined.
    if args.isEmpty then
      if let some d := (← read).declMap[fn]? then
        if let some d0 := isInlinedConstDecl? d then
          return ← emitLetValue d0
    if let some expr ← emitPrimitiveOp? fn args then
      return expr
    if let some expr ← emitArrayRead? fn args decl.type then
      return expr
    if let some expr ← emitExternCall? fn args decl.type then
      return expr
    if let some expr ← emitMemberCall? fn args then
      return expr
    let fnName := toKotlinFnName fn
    let monoDecl? ← getMonoDecl? fn
    let isAux := isCompilerAuxDecl (← getEnv) fn
    let numTypeParams ← (Meta.MetaM.run' do
      if isAux then return 0
      getDeclNumTypeParams (← getEnv) fn : CoreM Nat)
    let runtimeArgs := args.extract numTypeParams args.size
    let impureParams? : Option (Array (Param .impure)) ← match (← read).declMap[fn]? with
      | some d => pure (some d.params)
      | none => (·.map (·.params)) <$> getImpureSignature? fn
    let mut argStrs : Array String := #[]
    for i in [:runtimeArgs.size] do
      let fullIdx := numTypeParams + i
      let aStr ← toKotlinArg runtimeArgs[i]!
      let expectedTy? := match impureParams?.bind (·[fullIdx]?) with
        | some p =>
          let kt := toKotlinType p.type
          if kt != "Any?" then some kt else none
        | none => none
      let expectedTy? := expectedTy?.orElse fun _ =>
        match monoDecl?.bind (·.params[fullIdx]?) with
        | some mp => if isRichMonoType mp.type then some (toKotlinType mp.type) else none
        | none => none
      let aStr ← match expectedTy? with
        | some expectedTy => castIfNeeded aStr expectedTy
        | none => pure aStr
      argStrs := argStrs.push aStr
    let argStr := String.intercalate ", " argStrs.toList
    return s!"{fnName}({argStr})"
  | .fvar fvarId args =>
    let fnName ← getVarName fvarId
    let paramTys? := ((← get).varTypes[fnName]?).bind parseKotlinFnType? |>.map (·.1)
    let mut argStrs : Array String := #[]
    for i in [:args.size] do
      let aStr ← toKotlinArg args[i]!
      let aStr ← match paramTys?.bind (·[i]?) with
        | some pTy => castIfNeeded aStr pTy
        | none => pure aStr
      argStrs := argStrs.push aStr
    if let some paramTys := paramTys? then
      if args.size < paramTys.size then
        let mut lambdaParams : Array String := #[]
        for i in [args.size:paramTys.size] do
          let pName := s!"p_{i - args.size}"
          lambdaParams := lambdaParams.push s!"{pName}: {paramTys[i]!}"
          argStrs := argStrs.push pName
        let argStr := String.intercalate ", " argStrs.toList
        let paramStr := String.intercalate ", " lambdaParams.toList
        return s!"\{ {paramStr} -> {fnName}({argStr}) }"
      let argStr := String.intercalate ", " argStrs.toList
      return s!"{fnName}({argStr})"
    else
      let argStr := String.intercalate ", " argStrs.toList
      let anyParams := String.intercalate ", " (List.replicate args.size "Any?")
      return s!"({fnName} as ({anyParams}) -> Any?)({argStr})"
  | .pap fn args =>
    let targetFn ← unboxedPapFn fn
    let isAux := isCompilerAuxDecl (← getEnv) targetFn
    let numTypeParams ← (Meta.MetaM.run' do
      if isAux then return 0
      getDeclNumTypeParams (← getEnv) targetFn : CoreM Nat)
    let runtimeArgs := args.extract numTypeParams args.size
    let monoDecl? ← getMonoDecl? targetFn
    let impureParams? : Option (Array (Param .impure)) ← match (← read).declMap[targetFn]? with
      | some d => pure (some d.params)
      | none => (·.map (·.params)) <$> getImpureSignature? targetFn
    let numParams := match impureParams? with
      | some ps => ps.size
      | none => (monoDecl?.map (·.params.size)).getD args.size
    let runtimeNumParams := numParams - numTypeParams
    let paramTy (i : Nat) : String :=
      let fullIdx := numTypeParams + i
      let impTy := match impureParams?.bind (·[fullIdx]?) with
        | some p => toKotlinType p.type
        | none => "Any?"
      if impTy != "Any?" then impTy
      else match monoDecl?.bind (·.params[fullIdx]?) with
        | some mp => if isRichMonoType mp.type then toKotlinType mp.type else "Any?"
        | none => "Any?"
    let mut callArgs : Array String := #[]
    let mut fullArgs : Array (Arg .impure) := runtimeArgs
    for i in [:runtimeArgs.size] do
      let aStr ← toKotlinArg runtimeArgs[i]!
      let aStr ← castIfNeeded aStr (paramTy i)
      callArgs := callArgs.push aStr
    let inScope ← (← read).currParams.mapM fun p => getVarName p.fvarId
    let mut lambdaParams : Array String := #[]
    for i in [runtimeArgs.size:runtimeNumParams] do
      let idx := i - runtimeArgs.size
      let mut pName := s!"p_{idx}"
      if inScope.contains pName then
        pName := s!"arg_{idx}"
      let pTy := paramTy i
      lambdaParams := lambdaParams.push s!"{pName}: {pTy}"
      callArgs := callArgs.push pName
      let dummyFv : FVarId := { name := .num `_pap_param i }
      setParamVarName dummyFv pName
      recordVarType pName pTy
      fullArgs := fullArgs.push (.fvar dummyFv)
    let callStr ← match ← emitPrimitiveOp? targetFn fullArgs with
      | some primExpr => pure primExpr
      | none =>
        let fnName :=
          if numTypeParams > 0 then s!"{toKotlinFnName targetFn}___redArg"
          else toKotlinFnName targetFn
        pure s!"{fnName}({String.intercalate ", " callArgs.toList})"
    let paramStr := String.intercalate ", " lambdaParams.toList
    return s!"\{ {paramStr} -> {callStr} }"
  | .box _ fvarId =>
    getVarName fvarId
  | .unbox fvarId =>
    let src ← getVarName fvarId
    let srcTy := (← get).varTypes[src]?.getD "Any?"
    if isKotlinIntType srcTy then
      pure src
    else
      castIfNeeded src (toKotlinType decl.type)
  | .oproj i x =>
    if (← classStructOf? x).isSome then classField x (·.objs[i]?)
    else
      let xName ← getVarName x
      let xTy := (← get).varTypes[xName]?.getD "Any?"
      let targetTy := toKotlinType decl.type
      if (parseKotlinPairType? xTy).isSome then
        match i with
        | 0 => return s!"{xName}.first"
        | 1 => return s!"{xName}.second"
        | _ => unsupportedProj x
      else if xTy == "Array<Any?>" then
        castIfNeeded s!"{xName}[{1 + i}]" targetTy
      else if (← get).adtVars.contains xName || i >= 2 then
        castIfNeeded s!"({xName} as Array<*>)[{1 + i}]" targetTy
      else
        let prop := if i == 0 then "first" else "second"
        let recv := if xName.all (fun c => c.isAlphanum || c == '_') then xName else s!"({xName} as Pair<*, *>)"
        castIfNeeded s!"(if ({xName} is Pair<*, *>) {recv}.{prop} else ({xName} as Array<*>)[{1 + i}])" targetTy
  | .sproj n off x =>
    if (← classStructOf? x).isSome then classField x (·.scalars[off]?)
    else
      let xName ← getVarName x
      let xTy := (← get).varTypes[xName]?.getD "Any?"
      let targetTy := toKotlinType decl.type
      let elem := if xTy == "Array<Any?>" then s!"{xName}[{1 + n + off}]" else s!"({xName} as Array<*>)[{1 + n + off}]"
      castIfNeeded elem targetTy
  | .uproj i x =>
    if (← classStructOf? x).isSome then classField x (·.usizes[i]?)
    else
      let xName ← getVarName x
      let xTy := (← get).varTypes[xName]?.getD "Any?"
      let targetTy := toKotlinType decl.type
      let elem := if xTy == "Array<Any?>" then s!"{xName}[{1 + i}]" else s!"({xName} as Array<*>)[{1 + i}]"
      castIfNeeded elem targetTy
  | .ctor info args =>
    let env ← getEnv
    let inductName := match env.find? info.name with
      | some (.ctorInfo cv) => cv.induct
      | _ => info.name.getPrefix
    if Compiler.isMutableKotlinClass env inductName then
      throwError "Kotlin backend: in `{(← read).currFn}`: cannot allocate a new `{inductName}` (`@[mutable_kotlin_class]` values are only updated in place)"
    if let some spec := Compiler.getKotlinClassSpec? env inductName then
      let clsName := spec.baseClassName
      let variantName := toPascalCase info.name.getString!
      if Compiler.isKotlinEnum env inductName then
        return s!"{clsName}.{variantName}"
      else if Compiler.isKotlinInductive env inductName then
        let some (.ctorInfo cv) := env.find? info.name | unreachable!
        if cv.numFields == 0 then
          return s!"{clsName}.{variantName}"
        else
          let ctorArgs ← args.mapM toKotlinArg
          return s!"{clsName}.{variantName}({String.intercalate ", " ctorArgs.toList})"
      else
        let ctorArgs ← args.mapM toKotlinArg
        return s!"{clsName}({String.intercalate ", " ctorArgs.toList})"
    formatAdtCtor info (← args.mapM toKotlinArg) {} {}
  | _ => return "null"

/-- Continuation of RC and field-update instructions, which the Kotlin backend ignores. -/
def skipCont? : Code .impure → Option (Code .impure)
  | .del (k := k) .. | .dec (k := k) .. | .inc (k := k) .. | .setTag (k := k) ..
  | .sset (k := k) .. | .uset (k := k) .. | .oset (k := k) .. => some k
  | _ => none

/-- Strips leading RC instructions (`inc`, `dec`, `del`) that the Kotlin backend ignores. -/
partial def skipRC : Code .impure → Code .impure
  | .del (k := k) .. | .dec (k := k) .. | .inc (k := k) .. => skipRC k
  | c => c

def isCallTo (fn : Name) : LetValue .impure → Bool
  | .fap n .. => n == fn
  | _ => false

partial def countJmp (target : FVarId) (code : Code .impure) : Nat :=
  match code with
  | .let _ k => countJmp target k
  | .jp d k => countJmp target d.value + countJmp target k
  | .jmp fvarId _ => if fvarId == target then 1 else 0
  | .cases c => c.alts.foldl (fun acc alt => acc + countJmp target alt.getCode) 0
  | c => match skipCont? c with | some k => countJmp target k | none => 0

partial def hasBranches (code : Code .impure) : Bool :=
  match code with
  | .let _ k => hasBranches k
  | .oset (k := k) .. | .sset (k := k) .. | .uset (k := k) .. => hasBranches k
  | .return _ => false
  | c => match skipCont? c with | some k => hasBranches k | none => true

partial def needsJpLabel (target : FVarId) (code : Code .impure) : Bool :=
  match code with
  | .let _ k => needsJpLabel target k
  | .jp d k =>
    if countJmp d.fvarId k >= 2 && countJmp target k > 0 then true
    else needsJpLabel target d.value || needsJpLabel target k
  | .cases cs => cs.alts.any fun a => needsJpLabel target a.getCode
  | c => match skipCont? c with | some k => needsJpLabel target k | none => false

partial def hasSelfTailCall (fnName : Name) (code : Code .impure) : Bool :=
  match code with
  | .let decl k =>
    match skipRC k with
    | .return fvarId => decl.fvarId == fvarId && isCallTo fnName decl.value
    | _ => hasSelfTailCall fnName k
  | .cases c => c.alts.any fun alt => hasSelfTailCall fnName alt.getCode
  | c => match skipCont? c with | some k => hasSelfTailCall fnName k | none => false

partial def hasMultiUseJP (code : Code .impure) : Bool :=
  match code with
  | .let _ k => hasMultiUseJP k
  | .jp decl k =>
    if countJmp decl.fvarId k >= 2 then true
    else hasMultiUseJP decl.value || hasMultiUseJP k
  | .cases c => c.alts.any fun alt => hasMultiUseJP alt.getCode
  | c => match skipCont? c with | some k => hasMultiUseJP k | none => false

partial def hasRecursiveJP (code : Code .impure) : Bool :=
  match code with
  | .let _ k => hasRecursiveJP k
  | .jp decl k =>
    if countJmp decl.fvarId decl.value > 0 then true
    else hasRecursiveJP decl.value || hasRecursiveJP k
  | .cases c => c.alts.any fun alt => hasRecursiveJP alt.getCode
  | c => match skipCont? c with | some k => hasRecursiveJP k | none => false

partial def hasSelfCall (fnName : Name) (code : Code .impure) : Bool :=
  match code with
  | .let decl k => isCallTo fnName decl.value || hasSelfCall fnName k
  | .jp decl k => hasSelfCall fnName decl.value || hasSelfCall fnName k
  | .cases c => c.alts.any fun alt => hasSelfCall fnName alt.getCode
  | c => match skipCont? c with | some k => hasSelfCall fnName k | none => false

/-- Every self-call has the form `let x := f args; return x`. -/
partial def selfCallsAllTail (fnName : Name) (code : Code .impure) : Bool :=
  match code with
  | .let decl k =>
    if isCallTo fnName decl.value then
      match skipRC k with
      | .return r => r == decl.fvarId
      | _ => false
    else selfCallsAllTail fnName k
  | .jp decl k => selfCallsAllTail fnName decl.value && selfCallsAllTail fnName k
  | .cases c => c.alts.all fun alt => selfCallsAllTail fnName alt.getCode
  | c => match skipCont? c with | some k => selfCallsAllTail fnName k | none => true

partial def collectSelfCallArgs (fnName : Name) (code : Code .impure)
    (acc : Array (Array (Arg .impure))) : Array (Array (Arg .impure)) :=
  match code with
  | .let decl k =>
    let acc := match decl.value with
      | .fap n args => if n == fnName then acc.push args else acc
      | _ => acc
    collectSelfCallArgs fnName k acc
  | .jp decl k => collectSelfCallArgs fnName k (collectSelfCallArgs fnName decl.value acc)
  | .cases c => c.alts.foldl (fun acc alt => collectSelfCallArgs fnName alt.getCode acc) acc
  | c => match skipCont? c with | some k => collectSelfCallArgs fnName k acc | none => acc

/--
An `@[inline]` self-tail-recursive function (all self-calls in tail position, no recursive join
points) is emitted as a `while (true)` loop at each call site instead of being called.
-/
def isLoopExpandable (env : Environment) (decl : Decl .impure) : Bool :=
  match decl.value with
  | .code code =>
    Compiler.hasInlineAttribute env decl.name &&
    (Compiler.getKotlinMemberInfo? env decl.name).isNone &&
    hasSelfCall decl.name code && selfCallsAllTail decl.name code && !hasRecursiveJP code
  | _ => false

/--
Checks whether all uses of `x` in `code` are either as the callee of `.fvar x args` or passed to a
`loopExpandable` function, so `x` is beta-inlined at every use and does not need a Kotlin variable.
-/
partial def allUsesAreBetaInlined (env : Environment) (declMap : Std.HashMap Name (Decl .impure))
    (x : FVarId) (code : Code .impure) (papRemArity? : Option Nat := none)
    (visited : Std.HashSet Name := {}) : Bool :=
  let usesLV (v : LetValue .impure) : Bool :=
    ((v.forFVarM (m := StateM Bool) (fun f => if f == x then set true else pure ())).run false).2
  match code with
  | .inc (k := k) .. | .dec (k := k) .. | .del (k := k) .. =>
    allUsesAreBetaInlined env declMap x k papRemArity? visited
  | .let d k =>
    let okVal := match d.value with
      | .fvar fnFv args =>
        !args.any (· == .fvar x) &&
        (fnFv != x || match papRemArity? with | some rem => args.size == rem | none => true)
      | .fap fn args =>
        if !usesLV d.value then true
        else if visited.contains fn then true
        else match declMap[fn]? with
          | some callee =>
            if !isLoopExpandable env callee then false
            else match callee.value with
              | .code calleeBody =>
                let selfArgs := collectSelfCallArgs callee.name calleeBody #[]
                args.zipIdx.all fun (a, i) =>
                  if a != .fvar x then true
                  else match callee.params[i]? with
                    | some p =>
                      let isInvariant := selfArgs.all fun sa => sa[i]? == some (.fvar p.fvarId)
                      isInvariant && allUsesAreBetaInlined env declMap p.fvarId calleeBody papRemArity? (visited.insert fn)
                    | none => false
              | _ => false
          | none => false
      | v => !usesLV v
    okVal && allUsesAreBetaInlined env declMap x k papRemArity? visited
  | .jp d k | .fun d k _ =>
    allUsesAreBetaInlined env declMap x d.value papRemArity? visited && allUsesAreBetaInlined env declMap x k papRemArity? visited
  | .cases cs =>
    cs.discr != x && cs.alts.all fun alt => allUsesAreBetaInlined env declMap x alt.getCode papRemArity? visited
  | .jmp fn args => fn != x && !args.any (· == .fvar x)
  | .return f => f != x
  | .unreach _ => true
  | .oset y _ a k _ =>
    y != x && a != .fvar x && allUsesAreBetaInlined env declMap x k papRemArity? visited
  | .sset y _ _ z _ k _ | .uset y _ z k _ =>
    y != x && z != x && allUsesAreBetaInlined env declMap x k papRemArity? visited
  | .setTag y _ k _ =>
    y != x && allUsesAreBetaInlined env declMap x k papRemArity? visited

/-- Declarations called (or, if `paps`, also partially applied) in `code`. -/
partial def collectCalls (env : Environment) (declMap : Std.HashMap Name (Decl .impure))
    (code : Code .impure) (acc : Array Name) (paps := true) : Array Name :=
  match code with
  | .let decl k =>
    let acc := match decl.value with
      | .fap n _ => acc.push n
      | .pap n papArgs =>
        let unboxed := match n with | .str p "_boxed" => p | _ => n
        if paps then (acc.push n).push unboxed
        else
          let canBeta := match declMap[n]? with
            | some d =>
              match d.value with
              | .code c =>
                !hasRecursiveJP c && (!hasSelfCall d.name c || selfCallsAllTail d.name c) &&
                allUsesAreBetaInlined env declMap decl.fvarId k (some (d.params.size - papArgs.size))
              | _ => false
            | none => false
          if !canBeta then acc.push unboxed
          else acc
      | _ => acc
    collectCalls env declMap k acc paps
  | .jp decl k => collectCalls env declMap k (collectCalls env declMap decl.value acc paps) paps
  | .cases c => c.alts.foldl (fun acc alt => collectCalls env declMap alt.getCode acc paps) acc
  | c => match skipCont? c with | some k => collectCalls env declMap k acc paps | none => acc

/-- Declarations that are partially applied (closures) in `code`. -/
partial def collectPaps (code : Code .impure) (acc : Std.HashSet Name) : Std.HashSet Name :=
  match code with
  | .let decl k =>
    let acc := match decl.value with
      | .pap n _ => acc.insert n
      | _ => acc
    collectPaps k acc
  | .jp decl k => collectPaps k (collectPaps decl.value acc)
  | .cases c => c.alts.foldl (fun acc alt => collectPaps alt.getCode acc) acc
  | c => match skipCont? c with | some k => collectPaps k acc | none => acc

def loopExpandable? (v : LetValue .impure) : EmitM (Option (Decl .impure × Array (Arg .impure))) := do
  let .fap fn args := v | return none
  if let some l := (← read).loop? then
    if l.fnName == fn then return none
  let some d := (← read).declMap[fn]? | return none
  if isLoopExpandable (← getEnv) d then return some (d, args) else return none

/--
Application of a closure recorded in `State.paps`: the lambda declaration and the full argument
list (captured arguments followed by the application arguments).
-/
def papBeta? (v : LetValue .impure) : EmitM (Option (Decl .impure × Array (Arg .impure))) := do
  let .fvar f args := v | return none
  let some (fn, papArgs) := (← get).paps[← getVarName f]? | return none
  let some d := (← read).declMap[fn]? | return none
  let fullArgs := papArgs ++ args
  unless fullArgs.size == d.params.size do return none
  return some (d, fullArgs)

/-- Records `let x := pap fn args` of a local code declaration, emitting nothing if all uses are beta-inlined. -/
def recordPap? (decl : LetDecl .impure) (k : Code .impure) : EmitM Bool := do
  let .pap fn args := decl.value | return false
  let some d := (← read).declMap[fn]? | return false
  let .code c := d.value | return false
  unless !hasRecursiveJP c && (!hasSelfCall d.name c || selfCallsAllTail d.name c) do return false
  let remArity := d.params.size - args.size
  unless allUsesAreBetaInlined (← getEnv) (← read).declMap decl.fvarId k (some remArity) do return false
  let x ← getVarName decl.fvarId
  modify fun st => { st with paps := st.paps.insert x (fn, args) }
  return true

def currentExit : EmitM LoopExit := do
  return (← read).loop?.map (·.exit) |>.getD .funcReturn

def emitReturn (valStr : String) (valTy? : Option String := none) : EmitM Unit := do
  match ← currentExit with
  | .funcReturn =>
    if (← read).retUnit then
      -- Evaluate the value for its effects unless it is a plain variable or constant.
      unless valStr == "null" || valStr.all (fun c => c.isAlphanum || c == '_') do
        emitLn valStr
      emitLn "return"
    else
      match (← read).retCast? with
      | some t => emitLn s!"return {← castIfNeeded valStr t valTy?}"
      | none => emitLn s!"return {valStr}"
  | .inlineAlias fv =>
    if let some t := valTy? then recordVarType valStr t
    setParamVarName fv valStr
  | .assignOnly x xTy =>
    emitLn s!"{x} = {← castIfNeeded valStr xTy valTy?}"
  | .assignAndBreak x xTy loopLbl =>
    let rhs ← castIfNeeded valStr xTy valTy?
    modify fun st => { st with loopExits := st.loopExits.insert loopLbl ((st.loopExits.getD loopLbl #[]).push rhs) }
    emitLn s!"{x} = {rhs}"
    emitLn s!"break@{loopLbl}"
  | .breakOnly loopLbl =>
    unless valStr == "null" || valStr.all (fun c => c.isAlphanum || c == '_') do
      emitLn valStr
    emitLn s!"break@{loopLbl}"
  | .returnLbl lbl retTy? =>
    let rhs ← match retTy? with
      | some t => castIfNeeded valStr t valTy?
      | none => pure valStr
    emitLn s!"return@{lbl} {rhs}"
  | .threaded successExit failureLbl =>
    if valStr == "(-1)" || valStr == "-1" || valStr == "4294967295L" || valStr == "4294967295" || valStr == "null" then
      emitLn s!"break@{failureLbl}"
    else
      match successExit with
      | .funcReturn =>
        if (← read).retUnit then
          unless valStr == "null" || valStr.all (fun c => c.isAlphanum || c == '_') do
            emitLn valStr
          emitLn "return"
        else
          match (← read).retCast? with
          | some t => emitLn s!"return {← castIfNeeded valStr t valTy?}"
          | none => emitLn s!"return {valStr}"
      | .assignAndBreak x xTy loopLbl =>
        let rhs ← castIfNeeded valStr xTy valTy?
        modify fun st => { st with loopExits := st.loopExits.insert loopLbl ((st.loopExits.getD loopLbl #[]).push rhs) }
        emitLn s!"{x} = {rhs}"
        emitLn s!"break@{loopLbl}"
      | .assignOnly x xTy =>
        emitLn s!"{x} = {← castIfNeeded valStr xTy valTy?}"
      | .breakOnly loopLbl =>
        emitLn s!"break@{loopLbl}"
      | .inlineAlias fv =>
        if let some t := valTy? then recordVarType valStr t
        setParamVarName fv valStr
      | .returnLbl lbl retTy? =>
        let rhs ← match retTy? with
          | some t => castIfNeeded valStr t valTy?
          | none => pure valStr
        emitLn s!"return@{lbl} {rhs}"
      | .threaded .. =>
        emitLn s!"return {valStr}"

/-- Returns from the current `return` target without a value. -/
def emitReturnUnit : EmitM Unit := do
  match ← currentExit with
  | .funcReturn => emitLn "return"
  | .inlineAlias fv => setParamVarName fv "Unit"
  | .assignOnly x _ => emitLn s!"{x} = Unit"
  | .assignAndBreak _ _ loopLbl => emitLn s!"break@{loopLbl}"
  | .breakOnly loopLbl => emitLn s!"break@{loopLbl}"
  | .returnLbl lbl _ => emitLn s!"return@{lbl} Unit"
  | .threaded _ failureLbl => emitLn s!"break@{failureLbl}"

def resolveKnownBool (s : String) : EmitM String := do
  match (← get).knownBools[s]? with
  | some true => return "true"
  | some false => return "false"
  | none => return s

def formatKotlinPair (comps : Array String) (targetTy? : Option String := none) : EmitM (String × String) := do
  let c0 ← resolveKnownBool (comps[0]?.getD "null")
  let c1 ← resolveKnownBool (comps[1]?.getD "null")
  let targetPair? := targetTy?.bind parseKotlinPairType?
  let c0 ← match targetPair? with
    | some (t0, _) => castIfNeeded c0 t0
    | none => pure c0
  let c1 ← match targetPair? with
    | some (_, t1) => castIfNeeded c1 t1
    | none => pure c1
  let varTypes := (← get).varTypes
  let t0 := match targetPair? with
    | some (t0, _) => t0
    | none => varTypes[c0]?.getD "Any?"
  let t1 := match targetPair? with
    | some (_, t1) => t1
    | none => varTypes[c1]?.getD "Any?"
  let pairTy := s!"Pair<{t0}, {t1}>"
  let pairExpr := s!"Pair({c0}, {c1})"
  recordVarType pairExpr pairTy
  return (pairExpr, pairTy)

/-- `return x`, dropping the components of the result that are identical to parameters. -/
def emitReturnVar (x : FVarId) : EmitM Unit := do
  let rawName ← getVarName x
  let n ← resolveKnownBool rawName
  match (← read).retShape? with
  | some (.param _) => emitReturnUnit
  | some shape@(.prod _) =>
    if let some comps := (← get).tuples[n]? then
      match keptComps shape with
      | #[] => emitReturnUnit
      | #[c] =>
        let comp ← resolveKnownBool comps[c]!
        emitReturn comp (← get).varTypes[comp]?
      | _ =>
        let (pairExpr, pairTy) ← formatKotlinPair comps (← read).retCast?
        emitReturn pairExpr (some pairTy)
    else
      match keptComps shape with
      | #[_] => emitReturn n (← get).varTypes[n]?
      | _ => throwError "Kotlin backend: in `{(← read).currFn}`: expected a `Prod.mk` result"
  | _ =>
    if let some comps := (← get).tuples[n]? then
      if !(← get).materializedTuples.contains n then
        let targetTy? ← match ← currentExit with
          | .funcReturn => pure (← read).retCast?
          | .assignOnly _ xTy | .assignAndBreak _ xTy _ => pure (some xTy)
          | .returnLbl _ retTy? => pure retTy?
          | _ => pure none
        let (pairExpr, pairTy) ← formatKotlinPair comps targetTy?
        emitReturn pairExpr (some pairTy)
        return
    emitReturn n (← get).varTypes[n]?

/--
Registers the result `x` of a call with drop shape `shape`: `kept` is the Kotlin expression of the
returned component (if any), the other components are the corresponding arguments.
-/
def registerDropped (x : FVarId) (shape : Ownership.RetShape) (args : Array (Arg .impure))
    (kept : String) : EmitM Unit := do
  -- Components named after arguments share their Kotlin names; record their class structs so
  -- projections of the components resolve (see `classStructOf?`).
  let noteArg (j : Nat) : EmitM Unit := do
    if let some (.fvar a) := args[j]? then discard <| classStructOf? a
  match shape with
  | .param j =>
    noteArg j
    setParamVarName x (← toKotlinArg (args[j]?.getD .erased))
  | .prod comps =>
    let mut strs := #[]
    for c in comps do
      match c with
      | some j =>
        noteArg j
        strs := strs.push (← toKotlinArg (args[j]?.getD .erased))
      | none => strs := strs.push kept
    let n ← getVarName x
    modify fun st => { st with tuples := st.tuples.insert n strs }
  | _ => pure ()

/--
Field write `x.f = v` for runtime field `pos` of the `@[kotlin_class]` value `x`, if it is one.
Skipped if the field is known to hold `v` already (see `State.fieldVals`).
-/
def emitFieldWrite (x : FVarId) (pos : ClassLayout → Option String) (v : String) : EmitM Unit := do
  if (← mutableClassStructOf? x).isSome then
    let lhs ← classField x pos
    if (← get).fieldVals[lhs]? == some v then return
    emitLn s!"{lhs} = {v}"
    modify fun st => { st with fieldVals := st.fieldVals.insert lhs v }

/-- Forgets all known field values (see `State.fieldVals`). -/
def forgetFieldVals : EmitM Unit :=
  modify fun st => { st with fieldVals := {} }

/-- Runs `act` with the current known field values, and restores them afterwards. -/
def withFieldVals (act : EmitM α) : EmitM α := do
  let saved := (← get).fieldVals
  let r ← act
  modify fun st => { st with fieldVals := saved }
  return r

/-- A class field projection: kind (0 object, 1 scalar, 2 usize), position, and source value. -/
abbrev ProjInfo := Nat × Nat × FVarId

structure WriteBackState where
  acc : Std.HashSet (FVarId × FVarId) := {}
  /-- Projections valid at all the jumps to a join point seen so far. -/
  jmpProjs : Std.HashMap FVarId (Std.HashMap FVarId ProjInfo) := {}
  jpParams : Std.HashMap FVarId (Array FVarId) := {}

/--
Write-backs of `code`: pairs `(x, v)` where `x := ctor ...` is an in-place update of a
`@[kotlin_class]` value `y` (the unique source of its projected fields) and `v`, assigned to some
field of `x`, is the projection of the same field of `y`, with no call on `y` in between (the only
way, apart from the update itself, to change the fields of `y`). Such writes are no-ops.
-/
partial def collectWriteBacks (env : Environment) (isClassCtor : Name → Bool) (code : Code .impure)
    (projs : Std.HashMap FVarId ProjInfo) : StateM WriteBackState Unit := do
  let killArgs (projs : Std.HashMap FVarId ProjInfo) (args : Array (Arg .impure)) :=
    projs.filter fun _ (_, _, y) => !args.any (· == .fvar y)
  let rec scalars (x : FVarId) (c : Code .impure) (r : Array (Nat × Nat × FVarId)) :=
    match c with
    | .sset y _ off v _ c _ => scalars x c (if y == x then r.push (1, off, v) else r)
    | .uset y i v c _ => scalars x c (if y == x then r.push (2, i, v) else r)
    | .inc (k := c) .. | .dec (k := c) .. => scalars x c r
    | _ => r
  match code with
  | .let decl k =>
    let x := decl.fvarId
    match decl.value with
    | .oproj i y => collectWriteBacks env isClassCtor k (projs.insert x (0, i, y))
    | .sproj _ off y => collectWriteBacks env isClassCtor k (projs.insert x (1, off, y))
    | .uproj i y => collectWriteBacks env isClassCtor k (projs.insert x (2, i, y))
    | .reset _ y =>
      let projs := match projs[y]? with | some p => projs.insert x p | none => projs
      collectWriteBacks env isClassCtor k projs
    | .ctor info args | .reuse _ info _ args =>
      if !isClassCtor info.name.getPrefix then
        collectWriteBacks env isClassCtor k (killArgs projs args)
      else
        -- Field assignments `(kind, pos, value)`: object arguments, then the trailing `sset`/`uset`s.
        let objs := args.mapIdx fun i a => match a with
          | .fvar v => some (0, i, v)
          | _ => none
        let assigns := objs.filterMap id ++ scalars x k #[]
        let srcs := assigns.filterMap fun (_, _, v) => projs[v]?.map (·.2.2)
        let mut projs := projs
        if let some y := srcs[0]? then
          if srcs.all (· == y) then
            for (kind, pos, v) in assigns do
              if projs[v]? == some (kind, pos, y) then
                modify fun s => { s with acc := s.acc.insert (x, v) }
          -- The update changes the fields of its sources.
          projs := projs.filter fun _ (_, _, y') => !srcs.contains y'
        collectWriteBacks env isClassCtor k projs
    | .fap fn args =>
      if fn == arrOp "set!" || fn == arrOp "setIfInBounds" || fn == arrOp "uset" then
        let projs := match (args[1]? : Option (Arg .impure)) with
          | some (.fvar a) => match projs[a]? with | some p => projs.insert x p | none => projs
          | _ => projs
        collectWriteBacks env isClassCtor k (killArgs projs args)
      else if let some ai := Ownership.inplaceArg? env fn args then
        let projs := match args[ai]? with
          | some (.fvar a) => match projs[a]? with | some p => projs.insert x p | none => projs
          | _ => projs
        collectWriteBacks env isClassCtor k (killArgs projs args)
      else
        collectWriteBacks env isClassCtor k (killArgs projs args)
    | .pap _ args => collectWriteBacks env isClassCtor k (killArgs projs args)
    | .fvar .. => collectWriteBacks env isClassCtor k {}
    | _ => collectWriteBacks env isClassCtor k projs
  | .jp decl k =>
    -- The body runs after the jumps to it: it starts with the projections valid at all of them.
    modify fun s => { s with jpParams := s.jpParams.insert decl.fvarId (decl.params.map (·.fvarId)) }
    collectWriteBacks env isClassCtor k projs
    let atJumps := (← get).jmpProjs.getD decl.fvarId {}
    collectWriteBacks env isClassCtor decl.value atJumps
  | .fun decl k _ =>
    collectWriteBacks env isClassCtor decl.value {}
    collectWriteBacks env isClassCtor k projs
  | .cases cs => for alt in cs.alts do collectWriteBacks env isClassCtor alt.getCode projs
  | .inc (k := k) .. | .dec (k := k) .. | .del (k := k) .. | .setTag (k := k) ..
  | .oset (k := k) .. | .sset (k := k) .. | .uset (k := k) .. =>
    collectWriteBacks env isClassCtor k projs
  | .jmp fn args =>
    -- Parameters for which this jump passes a projection are projections too.
    let params := (← get).jpParams.getD fn #[]
    let mut here := projs
    for h : i in [:args.size] do
      if let .fvar a := args[i] then
        if let some p := projs[a]? then
          if let some q := params[i]? then here := here.insert q p
    modify fun s => { s with jmpProjs := s.jmpProjs.alter fn fun
      | none => some here
      | some prev => some (prev.filter fun v p => here[v]? == some p) }
  | .return .. | .unreach .. => pure ()

/--
Adds the variables used by `code` to `used`, except uses by RC instructions and write-backs (see
`collectWriteBacks`).
-/
partial def collectUsed (wb : Std.HashSet (FVarId × FVarId)) (code : Code .impure)
    (used : Std.HashSet FVarId) : Std.HashSet FVarId :=
  let addLV (v : LetValue .impure) (s : Std.HashSet FVarId) : Std.HashSet FVarId :=
    (v.forFVarM (m := StateM (Std.HashSet FVarId)) (fun f => modify (·.insert f))).run s |>.2
  let addArg (s : Std.HashSet FVarId) (a : Arg .impure) : Std.HashSet FVarId :=
    match a with
    | .fvar f => s.insert f
    | _ => s
  match code with
  | .inc (k := k) .. | .dec (k := k) .. | .del (k := k) .. => collectUsed wb k used
  | .let decl k =>
    match decl.value with
    | .ctor _ args =>
      collectUsed wb k <| args.foldl (init := used) fun s a => match a with
        | .fvar f => if wb.contains (decl.fvarId, f) then s else s.insert f
        | _ => s
    | .reuse y _ _ args =>
      collectUsed wb k <| args.foldl (init := used.insert y) fun s a => match a with
        | .fvar f => if wb.contains (decl.fvarId, f) then s else s.insert f
        | _ => s
    | .fap fn args =>
      if fn == arrOp "get!Internal" || fn == arrOp "get!InternalBorrowed" then
        collectUsed wb k (addArg (addArg used (args[2]?.getD .erased)) (args[3]?.getD .erased))
      else
        collectUsed wb k (addLV decl.value used)
    | v => collectUsed wb k (addLV v used)
  | .jp decl k | .fun decl k _ => collectUsed wb k (collectUsed wb decl.value used)
  | .cases cs => cs.alts.foldl (fun s alt => collectUsed wb alt.getCode s) (used.insert cs.discr)
  | .jmp fn args => args.foldl addArg (used.insert fn)
  | .return x => used.insert x
  | .unreach _ => used
  | .oset x _ y k _ => collectUsed wb k (addArg (used.insert x) y)
  | .sset x _ _ y _ k _ | .uset x _ y k _ =>
    collectUsed wb k (if wb.contains (x, y) then used.insert x else (used.insert x).insert y)
  | .setTag x _ k _ => collectUsed wb k (used.insert x)

def markUsed (code : Code .impure) : EmitM Unit := do
  let env ← getEnv
  let isClassCtor (s : Name) := Compiler.isMutableKotlinClass env s
  let wb := ((collectWriteBacks env isClassCtor code {}).run { acc := (← get).writeBacks }).2.acc
  modify fun st => { st with writeBacks := wb, used := collectUsed wb code st.used }

def isWriteBack (x v : FVarId) : EmitM Bool :=
  return (← get).writeBacks.contains (x, v)

def substFVar (s : Std.HashMap FVarId FVarId) (x : FVarId) : FVarId :=
  s.getD x x

def substArg (s : Std.HashMap FVarId FVarId) (a : Arg .impure) : Arg .impure :=
  match a with
  | .fvar x => .fvar (substFVar s x)
  | .erased => .erased

def substLetValue (s : Std.HashMap FVarId FVarId) (v : LetValue .impure) : LetValue .impure :=
  match v with
  | .lit _ | .erased => v
  | .oproj i x => .oproj i (substFVar s x)
  | .sproj n off x => .sproj n off (substFVar s x)
  | .uproj i x => .uproj i (substFVar s x)
  | .fap fn args => .fap fn (args.map (substArg s))
  | .pap fn args => .pap fn (args.map (substArg s))
  | .fvar fn args => .fvar (substFVar s fn) (args.map (substArg s))
  | .ctor info args => .ctor info (args.map (substArg s))
  | .box ty x => .box ty (substFVar s x)
  | .unbox x => .unbox (substFVar s x)
  | .isShared x => .isShared (substFVar s x)
  | .reset n x => .reset n (substFVar s x)
  | .reuse x info u args => .reuse (substFVar s x) info u (args.map (substArg s))

partial def extractLinearJmp? (jpId : FVarId) (c : Code .impure) :
    Option (Array (Arg .impure) × (Code .impure → Code .impure)) :=
  match c with
  | .jmp fn args =>
    if fn == jpId then some (args, id) else none
  | .inc x n ch p k u =>
    (extractLinearJmp? jpId k).map fun (args, wrap) => (args, fun body => .inc x n ch p (wrap body) u)
  | .dec x n ch p o k u =>
    (extractLinearJmp? jpId k).map fun (args, wrap) => (args, fun body => .dec x n ch p o (wrap body) u)
  | .del x k u =>
    (extractLinearJmp? jpId k).map fun (args, wrap) => (args, fun body => .del x (wrap body) u)
  | .let d k =>
    (extractLinearJmp? jpId k).map fun (args, wrap) => (args, fun body => .let d (wrap body))
  | .oset x i y k u =>
    (extractLinearJmp? jpId k).map fun (args, wrap) => (args, fun body => .oset x i y (wrap body) u)
  | .sset x i off y ty k u =>
    (extractLinearJmp? jpId k).map fun (args, wrap) => (args, fun body => .sset x i off y ty (wrap body) u)
  | .uset x i y k u =>
    (extractLinearJmp? jpId k).map fun (args, wrap) => (args, fun body => .uset x i y (wrap body) u)
  | .setTag x cidx k u =>
    (extractLinearJmp? jpId k).map fun (args, wrap) => (args, fun body => .setTag x cidx (wrap body) u)
  | _ => none

/--
Prunes the dead branch of `ExpandResetReuse`'s `isShared` checks (which evaluate to `false` for
`@[mutable_kotlin_class]` and `true` for all immutable types) and inlines single-jump linear join
points (`resetjp` / `reusejp`) so that constructor allocations stay contiguous with their scalar
field `.sset`/`.uset` writes and mutable in-place updates alias cleanly.
-/
partial def simplifyResetReuse (code : Code .impure) : EmitM (Code .impure) := do
  let kbRef ← IO.mkRef ({} : Std.HashMap FVarId Bool)
  let rec go (s : Std.HashMap FVarId FVarId) (c : Code .impure) :
      EmitM (Code .impure) := do
    match c with
    | .inc x n ch p k u => return .inc (substFVar s x) n ch p (← go s k) u
    | .dec x n ch p o k u => return .dec (substFVar s x) n ch p o (← go s k) u
    | .del x k u => return .del (substFVar s x) (← go s k) u
    | .let d k =>
      let v := substLetValue s d.value
      let d := { d with value := v }
      if let .isShared y := v then
        let isMut ← isMutableClassFVar y
        kbRef.modify (·.insert d.fvarId (!isMut))
      return .let d (← go s k)
    | .oset x i y k u =>
      return .oset (substFVar s x) i (substArg s y) (← go s k) u
    | .sset x i off y ty k u =>
      return .sset (substFVar s x) i off (substFVar s y) ty (← go s k) u
    | .uset x i y k u =>
      return .uset (substFVar s x) i (substFVar s y) (← go s k) u
    | .setTag x cidx k u =>
      return .setTag (substFVar s x) cidx (← go s k) u
    | .return x =>
      return .return (substFVar s x)
    | .unreach ty =>
      return .unreach ty
    | .jmp fn args =>
      return .jmp (substFVar s fn) (args.map (substArg s))
    | .fun d k u =>
      let val ← go s d.value
      return .fun (.mk d.fvarId d.binderName d.params d.type val) (← go s k) u
    | .jp d k =>
      let k' ← go s k
      let useCount := countJmp d.fvarId k'
      let isRec := countJmp d.fvarId d.value > 0
      if useCount == 0 then
        return k'
      else if useCount == 1 && !isRec then
        if let some (args, wrap) := extractLinearJmp? d.fvarId k' then
          let mut s' := s
          for h : i in [:d.params.size] do
            let pid := d.params[i].fvarId
            if let some (.fvar argFv) := args[i]? then
              let rootFv := substFVar s argFv
              s' := s'.insert pid rootFv
          let inlinedVal ← go s' d.value
          return wrap inlinedVal
      let val' ← go s d.value
      return .jp (.mk d.fvarId d.binderName d.params d.type val') k'
    | .cases cs =>
      let discr := substFVar s cs.discr
      if cs.alts.size == 2 then
        if let some b := (← kbRef.get)[discr]? then
          let (thenCode, thenWhen) := match cs.alts[0]! with
            | .ctorAlt info c => (c, info.cidx == 1)
            | .default c => (c, true)
          let elseCode := cs.alts[1]!.getCode
          return ← go s (if b == thenWhen then thenCode else elseCode)
      let alts' : Array (Alt .impure) ← cs.alts.mapM fun alt => do
        match alt with
        | .ctorAlt info c => return Alt.ctorAlt info (← go s c)
        | .default c => return Alt.default (← go s c)
      return .cases (.mk cs.typeName cs.resultType discr alts')
  go {} code

structure CtorFieldSets where
  objs : Array String := #[]
  usizes : Std.HashMap Nat String := {}
  scalars : Std.HashMap Nat String := {}
  cont : Code .impure := default
  deriving Inhabited

/--
Collects `.oset`, `.uset`, and `.sset` field writes to `x` at the head of `k` (skipping RC ops)
that initialize an immutable constructor allocation `let x := .ctor info args`, returning the
updated object, usize, and scalar field maps along with the remaining continuation after the writes.
-/
partial def collectCtorFieldSets (x : FVarId) (initObjs : EmitM (Array String)) (k : Code .impure) :
    EmitM CtorFieldSets := do
  let objs ← initObjs
  let rec loop (objs : Array String) (usizes : Std.HashMap Nat String) (scalars : Std.HashMap Nat String)
      (c : Code .impure) : EmitM CtorFieldSets := do
    match c with
    | .inc (k := k') .. | .dec (k := k') .. | .del (k := k') .. =>
      loop objs usizes scalars k'
    | .oset y i a k' _ =>
      if y == x then
        let v ← toKotlinArg a
        let objs := if i < objs.size then objs.set! i v else objs
        loop objs usizes scalars k'
      else
        return { objs, usizes, scalars, cont := c }
    | .uset y i z k' _ =>
      if y == x then
        let v ← getVarName z
        loop objs (usizes.insert i v) scalars k'
      else
        return { objs, usizes, scalars, cont := c }
    | .sset y _ off z _ k' _ =>
      if y == x then
        let v ← getVarName z
        loop objs usizes (scalars.insert off v) k'
      else
        return { objs, usizes, scalars, cont := c }
    | _ => return { objs, usizes, scalars, cont := c }
  loop objs {} {} k

structure AliasState where
  aliases : Std.HashMap FVarId FVarId := {}
  projs : Std.HashMap FVarId ProjInfo := {}
  /-- Components of `Prod` results that are aliases. -/
  comps : Std.HashMap (FVarId × Nat) FVarId := {}
  /-- Roots of the arguments passed to each join point parameter (`none`: not a variable). -/
  jmpArgs : Std.HashMap FVarId (Array (Option FVarId)) := {}
  /-- Variables in scope at each non-inlined join point parameter's declaration site. -/
  jpScope : Std.HashMap FVarId (Std.HashSet FVarId) := {}

/--
Variables of `code` that are emitted as Kotlin aliases of another variable, mapped to the root:
in-place updates of `@[mutable_kotlin_class]` values (aliases of the unique source of their projected
fields), results of calls identical to an argument (see `registerDropped`), and join point
parameters all of whose arguments are aliases of the same variable.
-/
partial def collectAliases (params : Array (Param .impure)) (code : Code .impure) :
    EmitM (Std.HashMap FVarId FVarId) := do
  let root (f : FVarId) : StateT AliasState EmitM FVarId := return (← get).aliases.getD f f
  let addAlias (x r : FVarId) : StateT AliasState EmitM Unit :=
    modify fun (s : AliasState) => { s with aliases := s.aliases.insert x r }
  let addProj (x : FVarId) (p : ProjInfo) : StateT AliasState EmitM Unit :=
    modify fun (s : AliasState) => { s with projs := s.projs.insert x p }
  let rec go (inScope : Std.HashSet FVarId) (c : Code .impure) : StateT AliasState EmitM Unit := do
    match c with
    | .let decl k =>
      let x := decl.fvarId
      match decl.value with
      | .oproj i y =>
        match (← get).comps[(y, i)]? with
        | some r => addAlias x r
        | none => addProj x (0, i, y)
      | .sproj _ off y => addProj x (1, off, y)
      | .uproj i y => addProj x (2, i, y)
      | .reset _ y | .reuse y .. => addAlias x (← root y)
      | .ctor info args =>
        if Compiler.isMutableKotlinClass (← getEnv) info.name.getPrefix then
          let vs := args.filterMap (fun a => match a with | .fvar f => some f | _ => none)
            ++ Ownership.ctorScalars x k
          let roots ← vs.mapM root
          let projs := (← get).projs
          let mut srcs := roots.filterMap fun v => projs[v]?.map (·.2.2)
          if srcs.isEmpty then
            for (_, _, y) in projs.values do
              if (← mutableClassStructOf? y) == some info.name.getPrefix then
                srcs := srcs.push y
          let mut rootSrcs : Array FVarId := #[]
          for y in srcs do
            let ry ← root y
            unless rootSrcs.contains ry do rootSrcs := rootSrcs.push ry
          if rootSrcs.size == 1 then
            addAlias x rootSrcs[0]!
      | .fap fn args =>
        if fn == arrOp "set!" || fn == arrOp "setIfInBounds" || fn == arrOp "uset" then
          if let some (.fvar y) := (args[1]? : Option (Arg .impure)) then addAlias x (← root y)
        else if fn == arrOp "propagateMark" || fn == `Array.propagateMark._redArg then
          if let some (.fvar y) := args.back? then addAlias x (← root y)
        else if let some ai := Ownership.inplaceArg? (← getEnv) fn args then
          if let some (.fvar y) := args[ai]? then addAlias x (← root y)
        else
          match ← dropShape? fn with
          | some (.param i) =>
            if let some (.fvar y) := args[i]? then addAlias x (← root y)
          | some (.prod comps) =>
            for h : c in [:comps.size] do
              if let some i := comps[c] then
                if let some (.fvar y) := args[i]? then
                  let r ← root y
                  modify fun (s : AliasState) => { s with comps := s.comps.insert (x, c) r }
          | _ => pure ()
      | _ => pure ()
      go (inScope.insert x) k
    | .jp decl k =>
      let isSingleUse := countJmp decl.fvarId k == 1 && countJmp decl.fvarId decl.value == 0
      let mut bodyScope := inScope
      for p in decl.params do
        unless isSingleUse do
          modify fun (s : AliasState) => { s with jpScope := s.jpScope.insert p.fvarId inScope }
        bodyScope := bodyScope.insert p.fvarId
      go bodyScope decl.value
      go inScope k
    | .fun decl k _ =>
      let bodyScope := decl.params.foldl (init := inScope) fun s p => s.insert p.fvarId
      go bodyScope decl.value
      go (inScope.insert decl.fvarId) k
    | .cases cs => for alt in cs.alts do go inScope alt.getCode
    | .jmp fn args =>
      let some (d : FunDecl .impure) ← findFunDecl? fn | return
      for h : i in [:d.params.size] do
        let r ← match args[i]? with
          | some (.fvar y) => some <$> root y
          | _ => pure none
        let pid := d.params[i].fvarId
        modify fun (s : AliasState) => { s with jmpArgs := s.jmpArgs.insert pid ((s.jmpArgs.getD pid #[]).push r) }
    | c => if let some k := skipCont? c then go inScope k
  -- Join point bodies precede their jumps: a multi-pass fixed point sees nested parameter aliases.
  let initScope := params.foldl (init := ({} : Std.HashSet FVarId)) fun s p => s.insert p.fvarId
  let mut seed : Std.HashMap FVarId FVarId := {}
  let mut aliases : Std.HashMap FVarId FVarId := {}
  for _ in [0:4] do
    let ((), st) ← (go initScope code).run ({ aliases := seed } : AliasState)
    aliases := st.aliases
    for (pid, rs) in st.jmpArgs do
      if let some (some r) := rs[0]? then
        let inJpScope := match st.jpScope[pid]? with
          | some sc => sc.contains r || sc.any fun v => aliases[v]? == some r
          | none => true
        if inJpScope && rs.all (· == some r) && r != pid then
          aliases := aliases.insert pid r
          seed := seed.insert pid r
  return aliases

def isUsed (x : FVarId) : EmitM Bool :=
  return (← get).used.contains x

def litKotlinType (v : LitValue) (ty : Expr) : String :=
  match v with
  | .uint8 _ =>
    if ty == ImpureType.bool then "Boolean"
    else if ty == ImpureType.int8 then "Byte"
    else "UByte"
  | .uint16 _ =>
    if ty == ImpureType.int16 then "Short"
    else "UShort"
  | .uint32 _ =>
    if ty == ImpureType.int32 then "Int"
    else "UInt"
  | .usize _ => "Int"
  | .nat _ => "java.math.BigInteger"
  | .uint64 _ =>
    if ty == ImpureType.int64 then "Long"
    else "ULong"
  | .str _ => "String"

def inferLetKotlinType (decl : LetDecl .impure) : EmitM String := do
  let baseTy := toKotlinType decl.type
  match decl.value with
  | .lit v => return litKotlinType v decl.type
  | .box _ fvarId =>
    return (← get).varTypes[← getVarName fvarId]?.getD baseTy
  | .unbox fvarId =>
    return (← get).varTypes[← getVarName fvarId]?.getD baseTy
  | .fap fn args =>
    let baseFn := match fn with | .str p "_boxed" => p | _ => fn
    let fnStr := baseFn.toString
    if baseFn == `panic || baseFn == `panic._redArg || baseFn == `panicCore || baseFn == `panicCore._redArg ||
       baseFn == `panicWithPos || baseFn == `panicWithPos._redArg ||
       baseFn == `panicWithPosWithDecl || baseFn == `panicWithPosWithDecl._redArg ||
       fnStr.startsWith "panic._at_." || fnStr.startsWith "panicWithPos._at_." ||
       fnStr.startsWith "panicWithPosWithDecl._at_." then
      return "Nothing"
    if baseFn == ``Thunk.mk || baseFn == `Thunk.mk._redArg ||
       baseFn == ``Thunk.pure || baseFn == `Thunk.pure._redArg then
      return "Lazy<Any?>"
    if baseFn == `ST.Prim.Ref.put || baseFn == `ST.Prim.Ref.put._redArg ||
       baseFn == `ST.Prim.Ref.set || baseFn == `ST.Prim.Ref.set._redArg then
      return "Unit"
    if baseFn == `ST.Prim.mkRef || baseFn == `ST.Prim.mkRef._redArg then
      return "Array<Any?>"
    if baseFn == `ST.Prim.Ref.ptrEq || baseFn == `ST.Prim.Ref.ptrEq._redArg then
      return "Boolean"
    if baseFn == `mixHash || baseFn == `String.hash || baseFn == `String.Slice.hash then
      return "ULong"
    if baseFn == `String.compare then
      return "Int"
    if baseFn == `String.Slice.instDecidableLt || baseFn == `ByteSlice.beq ||
       baseFn == `isExclusiveUnsafe || baseFn == `isExclusiveUnsafe._redArg then
      return "Boolean"
    if baseFn == `ptrAddrUnsafe || baseFn == `ptrAddrUnsafe._redArg then
      return "Int"
    if baseFn == `System.Platform.getNumBits || baseFn == `IO.monoMsNow ||
       baseFn == `IO.monoNanosNow || baseFn == `IO.getNumHeartbeats then
      return "java.math.BigInteger"
    if baseFn == `IO.initializing || baseFn == `IO.checkCanceled then
      return "Boolean"
    if baseFn == `IO.setNumHeartbeats || baseFn == `Runtime.forget || baseFn == `Runtime.forget._redArg ||
       baseFn == `Runtime.hold || baseFn == `Runtime.hold._redArg then
      return "Unit"
    if baseFn == `IO.getTID then
      return "ULong"
    if baseFn == `IO.Process.getPID then
      return "UInt"
    if baseFn == `IO.getEnv || baseFn == `IO.getStdout || baseFn == `IO.setStdout ||
       baseFn == `IO.getStderr || baseFn == `IO.setStderr ||
       baseFn == `IO.getStdin || baseFn == `IO.setStdin then
      return "Array<Any?>"
    let p := baseFn.getPrefix
    let s := match baseFn with | .str _ str => str | _ => ""
    if s == "toNat" || s == "toNatClampNeg" || s == "natAbs" then return "java.math.BigInteger"
    if s == "toInt64" then return "Long"
    if s == "toUInt64" then return "ULong"
    if s == "toInt32" || s == "toInt" || s == "toISize" || s == "toUSize" then return "Int"
    if s == "toUInt32" then return "UInt"
    if s == "toInt16" then return "Short"
    if s == "toUInt16" then return "UShort"
    if s == "toInt8" then return "Byte"
    if s == "toUInt8" then return "UByte"
    if s == "toFloat" then return "Double"
    if s == "toFloat32" then return "Float"
    if s == "toString" || s == "reprFast" || s == "repr" || baseFn == `mkPanicMessage || baseFn == `mkPanicMessageWithDecl then return "String"
    if p == ``Nat then
      if s.startsWith "dec" || s == "beq" || s == "blt" || s == "ble" then return "Boolean"
      else return "java.math.BigInteger"
    if p == ``Int64 || s == "shl64" || s == "ushr64" || s == "ashr64" then
      if s.startsWith "dec" then return "Boolean" else return "Long"
    if p == ``Int32 || s == "ushr32" then
      if s.startsWith "dec" then return "Boolean" else return "Int"
    if p == ``Int16 then
      if s.startsWith "dec" then return "Boolean" else return "Short"
    if p == ``Int8 then
      if s.startsWith "dec" then return "Boolean" else return "Byte"
    if p == ``UInt64 then
      if s.startsWith "dec" then return "Boolean" else return "ULong"
    if p == ``UInt32 then
      if s.startsWith "dec" then return "Boolean" else return "UInt"
    if p == ``UInt16 then
      if s.startsWith "dec" then return "Boolean" else return "UShort"
    if p == ``UInt8 then
      if s.startsWith "dec" then return "Boolean" else return "UByte"
    if p == ``ISize || p == ``USize then
      if s.startsWith "dec" then return "Boolean" else return "Int"
    if p == `Float then
      if s.startsWith "dec" || s == "beq" || s == "lt" || s == "le" || s.startsWith "is" then return "Boolean" else return "Double"
    if p == `Float32 then
      if s.startsWith "dec" || s == "beq" || s == "lt" || s == "le" || s.startsWith "is" then return "Boolean" else return "Float"
    if p == `Int then
      if s.startsWith "dec" then return "Boolean" else return "Int"
    if p == ``Char then
      if s == "toNat" || s == "utf8Size" then return "java.math.BigInteger"
      else if s == "ofNat" || s == "ofNatAux" then return "UInt"
    if p == `String || p == `String.Internal || p == `String.Pos || p == `String.Pos.Raw then
      if s.startsWith "dec" || s == "lt" || s == "isEmpty" || s == "isPrefixOf" || s == "atEnd" || s == "isValid" then return "Boolean"
      else if s == "length" || s == "utf8ByteSize" || s == "next" || s == "next'" || s == "prev" then return "java.math.BigInteger"
      else if s == "append" || s == "push" || s == "singleton" || s == "ofList" || s == "mk" || s == "extract" || s == "set" || s == "markLinear" || s == "propagateMark" || s == "fromUTF8!" || s == "fromUTF8" || s == "ofByteArray" then return "String"
      else if s == "decodeChar" || s == "get" || s == "get'" || s == "get!" then return "UInt"
      else if s == "getUTF8Byte" || s == "ugetUTF8Byte" then return "UByte"
      else if s == "toUTF8" || s == "toByteArray" then return "ByteArray"
      else if s == "toList" || s == "data" || s == "get?" then return "Array<Any?>"
    if baseFn == `List.asString then return "String"
    if p == ``ByteArray then
      if s == "size" then return "java.math.BigInteger"
      else if s == "usize" then return "Int"
      else if s == "isEmpty" || s == "validateUTF8" || s.startsWith "dec" then return "Boolean"
      else if s == "get!" || s == "get" || s == "uget" then return "UByte"
      else if s == "hash" then return "ULong"
      else if s == "data" then return if baseTy != "Any?" then baseTy else "UByteArray"
      else return "ByteArray"
    if p == ``FloatArray then
      if s == "size" then return "java.math.BigInteger"
      else if s == "usize" then return "Int"
      else if s == "isEmpty" || s.startsWith "dec" then return "Boolean"
      else if s == "get!" || s == "get" || s == "uget" then return "Double"
      else if s == "data" then return if baseTy != "Any?" then baseTy else "DoubleArray"
      else return "DoubleArray"
    if p == ``Bool then
      return "Boolean"
    if args.isEmpty then
      if let some d := (← read).declMap[fn]? then
        if let some d0 := isInlinedConstDecl? d then
          if let .lit v := d0.value then
            let effTy := if decl.type.isScalar then decl.type else d0.type
            return litKotlinType v effTy
    if baseFn == arrOp "get!Internal" || baseFn == arrOp "get!InternalBorrowed" then
      let arrTy ← arrayType (args[2]?.getD .erased)
      return (kotlinArrayElem? arrTy).getD baseTy
    if baseFn == arrOp "getInternal" || baseFn == arrOp "getInternalBorrowed" ||
       baseFn == arrOp "uget" || baseFn == arrOp "ugetBorrowed" then
      let arrTy ← arrayType (args[1]?.getD .erased)
      return (kotlinArrayElem? arrTy).getD baseTy
    if baseFn.isStr && baseFn.getString! == "aget" then
      let arrTy ← arrayType (args[args.size - 2]?.getD .erased)
      return (kotlinArrayElem? arrTy).getD baseTy
    if baseFn == arrOp "push" || baseFn == arrOp "pop" then
      return ← arrayType (args[1]?.getD .erased)
    if baseFn == arrOp "propagateMark" || baseFn == `Array.propagateMark._redArg ||
       baseFn == arrOp "markLinear" || baseFn == `Array.markLinear._redArg then
      return ← arrayType (args[args.size - 1]?.getD .erased)
    if baseFn == arrOp "append" || baseFn == `Array.append._redArg ||
       baseFn == arrOp "appendCore" || baseFn == `Array.appendCore._redArg then
      return ← arrayType (args[args.size - 2]?.getD .erased)
    if baseFn == arrOp "extract" || baseFn == `Array.extract._redArg then
      return ← arrayType (args[args.size - 3]?.getD .erased)
    if baseFn == arrOp "size" then
      return "java.math.BigInteger"
    if baseFn == arrOp "usize" then
      return "Int"
    if baseFn == arrOp "toList" then
      return "Array<Any?>"
    if baseFn == arrOp "mk" || baseFn == ``List.toArray || baseFn == `List.toArrayImpl || baseFn == `List.toArrayImpl._redArg ||
       baseFn == arrOp "replicate" || baseFn == arrOp "mkArray" || baseFn == arrOp "mkEmpty" || baseFn == arrOp "emptyWithCapacity" then
      return if baseTy != "Any?" then baseTy else "Array<Any?>"
    if baseTy != "Any?" then return baseTy
    fnRetKotlinType fn decl.type
  | .fvar fvarId args =>
    if baseTy != "Any?" then return baseTy
    let fnName ← getVarName fvarId
    if let some fnTy := (← get).varTypes[fnName]? then
      if let some (paramTys, retTy) := parseKotlinFnType? fnTy then
        if args.size < paramTys.size then
          let remParams := (paramTys.extract args.size paramTys.size).toList
          return s!"({String.intercalate ", " remParams}) -> {retTy}"
        return retTy
    return baseTy
  | .pap fn args =>
    let targetFn ← unboxedPapFn fn
    let monoDecl? ← getMonoDecl? targetFn
    let impureParams? : Option (Array (Param .impure)) ← match (← read).declMap[targetFn]? with
      | some d => pure (some d.params)
      | none => (·.map (·.params)) <$> getImpureSignature? targetFn
    let numParams := match impureParams? with
      | some ps => ps.size
      | none => (monoDecl?.map (·.params.size)).getD args.size
    let paramTy (i : Nat) : String :=
      let impTy := match impureParams?.bind (·[i]?) with
        | some p => toKotlinType p.type
        | none => "Any?"
      if impTy != "Any?" then impTy
      else match monoDecl?.bind (·.params[i]?) with
        | some mp => if isRichMonoType mp.type then toKotlinType mp.type else "Any?"
        | none => "Any?"
    let mut remParams : Array String := #[]
    for i in [args.size:numParams] do
      remParams := remParams.push (paramTy i)
    let defRetTy := match (← read).declMap[targetFn]? with
      | some d => d.type
      | none => decl.type
    let retTy ← fnRetKotlinType targetFn defRetTy
    return s!"({String.intercalate ", " remParams.toList}) -> {retTy}"
  | .oproj i x =>
    if let some s ← classStructOf? x then
      let layout ← classLayout s
      if let some fTy := layout.objTypes[i]? then
        if layout.typeParams.contains fTy then return "Any?"
        if fTy != "Any?" then return fTy
    let xName ← getVarName x
    if let some xTy := (← get).varTypes[xName]? then
      if let some (t0, t1) := parseKotlinPairType? xTy then
        if i == 0 then return t0
        if i == 1 then return t1
    return baseTy
  | _ => return baseTy

def isPureConstantLet (decl : LetDecl .impure) : EmitM Bool := do
  match decl.value with
  | .lit _ | .erased => return true
  | .fap fn args =>
    if fn.isStr && (fn.getString!.startsWith "instInhabited" || fn.getString!.contains "inhabited") then
      return true
    if args.isEmpty then
      if let some d := (← read).declMap[fn]? then
        if let some _ := isInlinedConstDecl? d then
          return true
    return false
  | _ => return false

def tryAliasLet? (decl : LetDecl .impure) : EmitM Bool := do
  let x := decl.fvarId
  match decl.value with
  | .lit v =>
    match v with
    | .str _ => return false
    | _ =>
      let s ← emitLetValue decl
      recordVarType s (litKotlinType v decl.type)
      setParamVarName x s
      return true
  | .fvar fvarId #[] =>
    let src ← getVarName fvarId
    if let some vt := (← get).varTypes[src]? then
      recordVarType src vt
    setParamVarName x src
    return true
  | .erased =>
    setParamVarName x "null"
    return true
  | .box _ fvarId =>
    setParamVarName x (← getVarName fvarId)
    return true
  | .unbox fvarId =>
    let src ← getVarName fvarId
    let targetTy := toKotlinType decl.type
    if (← get).varTypes[src]? == some targetTy then
      setParamVarName x src
      return true
    return false
  | .oproj i var =>
    let n ← getVarName var
    if let some comps := (← get).tuples[n]? then
      if let some c := comps[i]? then
        setParamVarName x c
        return true
    return false
  | .sproj _ _ var =>
    let n ← getVarName var
    if let some comps := (← get).tuples[n]? then
      if let some c := comps[0]? then
        setParamVarName x c
        return true
    return false
  | .uproj _ var =>
    let n ← getVarName var
    if let some comps := (← get).tuples[n]? then
      if let some c := comps[0]? then
        setParamVarName x c
        return true
    return false
  | .fap fn args =>
    let baseFn := match fn with | .str p "_boxed" => p | _ => fn
    if baseFn == `Void.mk || baseFn == `Void.mk._redArg then
      setParamVarName x "null"
      return true
    if baseFn == arrOp "propagateMark" || baseFn == `Array.propagateMark._redArg ||
       baseFn == `ByteArray.propagateMark || baseFn == `FloatArray.propagateMark ||
       baseFn == `String.propagateMark ||
       baseFn == `dbgTraceIfShared || baseFn == `dbgTraceIfShared._redArg then
      if let some lastArg := args.back? then
        setParamVarName x (← toKotlinArg lastArg)
        return true
    if (baseFn == `Runtime.markMultiThreaded || baseFn == `Runtime.markMultiThreaded._redArg ||
        baseFn == `Runtime.markPersistent || baseFn == `Runtime.markPersistent._redArg) && !args.isEmpty then
      let idx := if args.size >= 2 then args.size - 2 else 0
      setParamVarName x (← toKotlinArg args[idx]!)
      return true
    if args.isEmpty then
      if let some d := (← read).declMap[fn]? then
        if let some d0 := isInlinedConstDecl? d then
          if let .lit v := d0.value then
            if !(v matches .str _) then
              let effTy := if decl.type.isScalar then decl.type else d0.type
              let s ← emitLetValue { d0 with type := effTy }
              recordVarType s (litKotlinType v effTy)
              setParamVarName x s
              return true
    let env ← getEnv
    if let some extStr := getExternNameFor env `kotlin fn then
      if extStr == "kotlin_expr:null" then
        setParamVarName x "null"
        return true
      if extStr == "kotlin_op:id" then
        if let some a := args.back? then
          setParamVarName x (← toKotlinArg a)
          return true
      if extStr == "kotlin_op:cast" then
        if let some a := args.back? then
          let a0 ← toKotlinArg a
          let targetTy := toKotlinType decl.type
          if targetTy == "Any?" || (← get).varTypes[a0]? == some targetTy then
            setParamVarName x a0
            return true
    if args.size == 1 then
      let a0 ← toKotlinArg args[0]!
      let vt := (← get).varTypes[a0]?
      if vt == some "Int" || vt == some "UInt" then
        match fn with
        | ``Int32.ofInt | ``ISize.ofInt | `Int32.toInt
        | `Int.toInt32 | ``Int32.toISize | ``ISize.toInt32
        | ``UInt32.toNat | ``Int32.toNatClampNeg | ``USize.toNat =>
          let intExpr := if vt == some "UInt" then s!"({a0}).toInt()" else a0
          setParamVarName x intExpr
          modify fun st => { st with intSources := st.intSources.insert intExpr intExpr }
          return true
        | _ => pure ()
    return false
  | _ => return false

/--
`val x = y.f` for a projection of `y`. For a `@[kotlin_class]` value, the read is dropped if `x` is
unused, and the field is recorded as held by `x` (see `State.fieldVals`).
-/
def emitFieldRead (x y : FVarId) (pos : ClassLayout → Option String) (decl : LetDecl .impure) :
    EmitM Unit := do
  if (← classStructOf? y).isSome then
    let ty ← inferLetKotlinType decl
    let n ← getVarName x
    recordVarType n ty
    modify fun st => { st with projSrc := st.projSrc.insert n y }
    unless ← isUsed x do return
    let lhs ← classField y pos
    if let some prevVar := (← get).fieldVals[lhs]? then
      setParamVarName x prevVar
      return
    emitLn s!"val {n} = {lhs}"
    modify fun st => { st with fieldVals := st.fieldVals.insert lhs n }
  else
    unless ← isUsed x do return
    let n ← getVarName x
    let ty ← inferLetKotlinType decl
    recordVarType n ty
    emitLn s!"val {n} = {← emitLetValue decl}"

def withKnown (name : String) (b : Bool) (act : EmitM Unit) : EmitM Unit := do
  let saved := (← get).knownBools
  modify fun st => { st with knownBools := st.knownBools.insert name b }
  act
  modify fun st => { st with knownBools := saved }

def identTokens (s : String) : Std.HashSet String :=
  let (set, cur) := s.foldl (init := (({} : Std.HashSet String), "")) fun (set, cur) c =>
    if c.isAlphanum || c == '_' then (set, cur.push c)
    else (if cur.isEmpty then set else set.insert cur, "")
  if cur.isEmpty then set else set.insert cur

/-- Self tail call inside a loop-expanded body: update the loop variables and `continue`. -/
def emitLoopContinue (l : LoopCtx) (args : Array (Arg .impure)) : EmitM Unit := do
  let mut updates : Array (String × String) := #[]
  for i in [:l.varNames.size] do
    if l.variant[i]! then
      let a ← toKotlinArg (args[i]?.getD .erased)
      if a != l.varNames[i]! then
        updates := updates.push (l.varNames[i]!, a)
  let mut written : Std.HashSet String := {}
  let mut needTemps := false
  for (v, a) in updates do
    if (identTokens a).any (written.contains ·) then
      needTemps := true
    written := written.insert v
  if needTemps then
    for (v, a) in updates do emitLn s!"val {v}_t = {a}"
    for (v, _) in updates do emitLn s!"{v} = {v}_t"
  else
    for (v, a) in updates do emitLn s!"{v} = {a}"
  emitLn s!"continue@{l.loopLbl}"

def countUsesArg (x : FVarId) (a : Arg .impure) : Nat :=
  match a with
  | .fvar f => if f == x then 1 else 0
  | _ => 0

def countUsesLetValue (x : FVarId) (v : LetValue .impure) : Nat :=
  let (_, n) := (v.forFVarM (m := StateM Nat) (fun f => if f == x then modify (· + 1) else pure ())).run 0
  n

partial def countUsesCode (x : FVarId) (code : Code .impure) : Nat :=
  match code with
  | .inc (k := k) .. | .dec (k := k) .. | .del (k := k) .. => countUsesCode x k
  | .let decl k => countUsesLetValue x decl.value + countUsesCode x k
  | .jp decl k | .fun decl k _ => countUsesCode x decl.value + countUsesCode x k
  | .cases cs =>
    if cs.discr == x && cs.typeName == ``Bool && cs.alts.size == 2 then 1
    else
      (if cs.discr == x then 1 else 0) +
      cs.alts.foldl (fun acc alt => acc + countUsesCode x alt.getCode) 0
  | .jmp fn args =>
    (if fn == x then 1 else 0) +
    args.foldl (fun acc a => acc + countUsesArg x a) 0
  | .return f => if f == x then 1 else 0
  | .unreach _ => 0
  | .oset y _ a k _ => (if y == x then 1 else 0) + countUsesArg x a + countUsesCode x k
  | .sset y _ _ z _ k _ | .uset y _ z k _ =>
    (if y == x then 1 else 0) + (if z == x then 1 else 0) + countUsesCode x k
  | .setTag y _ k _ => (if y == x then 1 else 0) + countUsesCode x k

/--
Checks whether all uses of a `Prod.mk` variable `x` in `code` are either `.oproj _ x` or `.return x`,
so `x` stays in `State.tuples` without emitting a `val x = Pair(...)` declaration.
-/
partial def onlyUsedAsVirtualTuple (x : FVarId) (code : Code .impure) : Bool :=
  match code with
  | .inc (k := k) .. | .dec (k := k) .. | .del (k := k) .. => onlyUsedAsVirtualTuple x k
  | .let d k =>
    let okVal := match d.value with
      | .oproj _ _ => true
      | v => countUsesLetValue x v == 0
    okVal && onlyUsedAsVirtualTuple x k
  | .jp d k | .fun d k _ =>
    onlyUsedAsVirtualTuple x d.value && onlyUsedAsVirtualTuple x k
  | .cases cs =>
    cs.discr != x && cs.alts.all fun alt => onlyUsedAsVirtualTuple x alt.getCode
  | .jmp fn args => fn != x && !args.any (· == .fvar x)
  | .return _ => true
  | .unreach _ => true
  | .oset y _ a k _ => y != x && a != .fvar x && onlyUsedAsVirtualTuple x k
  | .sset y _ _ z _ k _ | .uset y _ z k _ => y != x && z != x && onlyUsedAsVirtualTuple x k
  | .setTag y _ k _ => y != x && onlyUsedAsVirtualTuple x k

def isPrimitiveOp (fn : Name) (numArgs : Nat) : Bool :=
  let fn := match fn with | .str p "_boxed" => p | _ => fn
  let p := fn.getPrefix
  let s := match fn with | .str _ str => str | _ => ""
  if p == ``Int8 || p == ``Int16 || p == ``Int32 || p == ``Int64 || p == ``ISize ||
     p == ``UInt8 || p == ``UInt16 || p == ``UInt32 || p == ``UInt64 || p == ``USize then
    if numArgs == 1 then
      s.startsWith "to" || s.startsWith "of" || s == "neg" || s == "complement" || s == "log2" || s == "abs"
    else if numArgs == 2 then
      s == "add" || s == "sub" || s == "mul" || s == "div" || s == "mod" ||
      s == "land" || s == "lor" || s == "xor" || s == "lxor" ||
      s == "shiftLeft" || s == "shiftRight" ||
      s == "decEq" || s == "decLt" || s == "decLe" || s == "ofNatLT"
    else false
  else if p == `Float || p == `Float32 then
    if numArgs == 1 then
      s.startsWith "to" || s.startsWith "of" || s.startsWith "is" ||
      s == "neg" || s == "abs" || s == "sqrt" || s == "sin" || s == "cos" || s == "tan" ||
      s == "asin" || s == "acos" || s == "atan" || s == "sinh" || s == "cosh" || s == "tanh" ||
      s == "asinh" || s == "acosh" || s == "atanh" || s == "exp" || s == "exp2" ||
      s == "log" || s == "log2" || s == "log10" || s == "cbrt" || s == "floor" || s == "ceil" || s == "round"
    else if numArgs == 2 then
      s == "add" || s == "sub" || s == "mul" || s == "div" ||
      s == "beq" || s == "decLt" || s == "lt" || s == "decLe" || s == "le" ||
      s == "pow" || s == "atan2" || s == "scaleB" || s == "minimum" || s == "maximum" || s == "minimumNumber" || s == "maximumNumber"
    else if numArgs == 3 then
      s == "ofScientific" || s == "fma"
    else false
  else if p == `String || p == `String.Internal || p == `String.Pos || p == `String.Pos.Raw || p == `String.Slice then
    if numArgs == 1 then
      s == "length" || s == "utf8ByteSize" || s == "isEmpty" || s == "hash" || s == "singleton" || s == "ofList" || s == "mk" || s == "toList" || s == "data" || s == "markLinear" || s == "toUTF8" || s == "toByteArray" || s == "fromUTF8!"
    else if numArgs == 2 then
      s == "append" || s == "push" || s == "decEq" || s == "decLt" || s == "decidableLT" || s == "lt" || s == "compare" || s == "instDecidableLt" || s == "isPrefixOf" ||
      s == "propagateMark" || s == "getUTF8Byte" || s == "ugetUTF8Byte" || s == "isValid" || s == "atEnd" ||
      s == "decodeChar" || s == "get" || s == "get'" || s == "get!" || s == "get?" || s == "next" || s == "next'" || s == "prev" ||
      s == "fromUTF8" || s == "ofByteArray"
    else if numArgs == 3 then
      s == "extract" || s == "set"
    else false
  else if p == ``ByteArray || p == `ByteSlice then
    if numArgs == 1 then s == "size" || s == "usize" || s == "isEmpty" || s == "hash" || s == "validateUTF8"
    else if numArgs == 2 then s == "get!" || s == "beq" || s == "decEq"
    else if numArgs == 3 then s == "get" || s == "uget"
    else false
  else if p == ``FloatArray then
    if numArgs == 1 then s == "size" || s == "usize" || s == "isEmpty"
    else if numArgs == 2 then s == "get!"
    else if numArgs == 3 then s == "get" || s == "uget"
    else false
  else if p == ``Char then
    numArgs == 1 && (s == "toNat" || s == "ofNat" || s == "ofNatAux" || s == "utf8Size")
  else if p == ``Nat || p == `Int then
    if numArgs == 1 then
      s.startsWith "to" || s.startsWith "of" || s == "shiftLeft" || s == "add" || s == "sub" || s == "mul" || s == "div" || s == "mod" ||
      s == "neg" || s == "negSucc" || s == "negOfNat" || s == "natAbs" || s == "decNonneg" ||
      s == "pred" || s == "log2" || s == "reprFast" || s == "repr"
    else if numArgs >= 2 then
      s == "decEq" || s == "decLt" || s == "decLe" || s == "beq" || s == "blt" || s == "ble" ||
      s == "land" || s == "lor" || s == "xor" || s == "shiftLeft" || s == "shiftRight" ||
      s == "add" || s == "sub" || s == "mul" || s == "div" || s == "divExact" || s == "mod" || s == "modCore" || s == "pow" || s == "gcd" ||
      s == "tdiv" || s == "tmod" || s == "ediv" || s == "emod"
    else false
  else if p == ``Bool then
    if numArgs == 1 then s.startsWith "to"
    else if numArgs == 2 then s == "decEq"
    else false
  else if fn == `mixHash && numArgs == 2 then true
  else if fn == `System.Platform.getNumBits then true
  else if fn == `isExclusiveUnsafe || fn == `isExclusiveUnsafe._redArg ||
          fn == `ptrAddrUnsafe || fn == `ptrAddrUnsafe._redArg then true
  else if fn == `UInt8.ofNatLT._redArg || fn == `UInt16.ofNatLT._redArg ||
          fn == `UInt32.ofNatLT._redArg || fn == `UInt64.ofNatLT._redArg ||
          fn == `USize.ofNatLT._redArg || fn == `Int.divExact._redArg ||
          fn == `Nat.divExact._redArg then true
  else
    if numArgs == 1 && fn.isStr && (s == "ofNat" || s == "toUInt64" || s == "toUInt32") then true
    else false

def isPureLet (decl : LetDecl .impure) : EmitM Bool := do
  if decl.value matches .pap .. then return true
  match decl.value with
  | .oproj _ y =>
    if (← classStructOf? y).isNone && !(← get).tuples.contains (← getVarName y) then
      return true
  | .sproj _ _ y | .uproj _ y =>
    if (← classStructOf? y).isNone then
      return true
  | _ => pure ()
  let .fap fn args := decl.value | return false
  if fn.isStr && (fn.getString!.startsWith "instInhabited" || fn.getString!.contains "inhabited" || fn.getString!.contains "boxed_const") then return true
  if (← loopExpandable? decl.value).isSome then return false
  if Ownership.isArraySet fn || Ownership.isScalarArraySet fn then return false
  if (Ownership.inplaceArg? (← getEnv) fn args).isSome then return false
  if (← dropShape? fn).isSome then return false
  if isPrimitiveOp fn args.size then return true
  let baseFn := match fn with | .str p "_boxed" => p | _ => fn
  if baseFn == arrOp "size" || baseFn == arrOp "usize" ||
     baseFn == arrOp "get!Internal" || baseFn == arrOp "get!InternalBorrowed" ||
     baseFn == arrOp "getInternal" || baseFn == arrOp "getInternalBorrowed" ||
     baseFn == arrOp "uget" || baseFn == arrOp "ugetBorrowed" then
    return true
  let env ← getEnv
  if let some extStr := getExternNameFor env `kotlin fn then
    if extStr.startsWith "kotlin_expr:" then return true
    if extStr == "kotlin_op:cast" || extStr == "kotlin_op:id" ||
       extStr.startsWith "kotlin_op:is:" || extStr.startsWith "kotlin_op:get:" then
      return true
  return false

def isInlineableLet (decl : LetDecl .impure) : EmitM Bool := do
  unless ← isUsed decl.fvarId do return false
  if (← get).varNames.contains decl.fvarId then return false
  if (← get).aliases.values.contains decl.fvarId then return false
  isPureLet decl

def mayAliasArg (decl : LetDecl .impure) : EmitM Bool := do
  match decl.value with
  | .box .. | .unbox .. => return true
  | .fap fn args =>
    if args.size != 1 then return false
    let env ← getEnv
    if let some extStr := getExternNameFor env `kotlin fn then
      if extStr == "kotlin_op:id" || extStr == "kotlin_op:cast" then
        return true
    match fn with
    | ``Int32.ofInt | ``ISize.ofInt | `Int32.toInt
    | `Int.toInt32 | ``Int32.toISize | ``ISize.toInt32
    | ``UInt32.toNat | ``Int32.toNatClampNeg | ``USize.toNat => return true
    | _ => return false
  | _ => return false

partial def usedBeforeSideEffect (x : FVarId) (code : Code .impure) : EmitM Bool := do
  match code with
  | .inc (k := k) .. | .dec (k := k) .. | .del (k := k) .. =>
    usedBeforeSideEffect x k
  | .return f =>
    return f == x
  | .cases cs =>
    return cs.discr == x
  | .jp decl k =>
    if countUsesCode x decl.value == 0 then
      usedBeforeSideEffect x k
    else
      return false
  | .sset y _ _ z _ k _ | .uset y _ z k _ =>
    if z == x then return y != x
    else if y != x then usedBeforeSideEffect x k
    else return false
  | .let d2 k2 =>
    if (← loopExpandable? d2.value).isSome || (← papBeta? d2.value).isSome then
      let u := countUsesLetValue x d2.value
      return u == 1 && countUsesCode x k2 == 0
    if d2.value matches .pap .. then
      return false
    let u := countUsesLetValue x d2.value
    if u > 0 then
      if u != 1 then return false
      match d2.value with
      | .box .. | .unbox .. =>
        return countUsesCode d2.fvarId k2 == 1 && (← usedBeforeSideEffect d2.fvarId k2)
      | .oproj _ y | .sproj _ _ y | .uproj _ y =>
        if (← classStructOf? y).isNone then
          return countUsesCode d2.fvarId k2 == 1 && (← usedBeforeSideEffect d2.fvarId k2)
        else
          return false
      | .ctor info _ =>
        if info.name == ``Prod.mk && (skipRC k2 == .return d2.fvarId) then
          return true
        return !Compiler.isMutableKotlinClass (← getEnv) info.name.getPrefix &&
          countUsesCode d2.fvarId k2 == 1 && (← usedBeforeSideEffect d2.fvarId k2)
      | .fap fn2 args2 =>
        if ← mayAliasArg d2 then
          return countUsesCode d2.fvarId k2 == 1 && (← usedBeforeSideEffect d2.fvarId k2)
        if Ownership.isArraySet fn2 then
          return args2[1]? != some (.fvar x)
        if let some ai := Ownership.inplaceArg? (← getEnv) fn2 args2 then
          return args2[ai]? != some (.fvar x)
        if (← dropShape? fn2).isSome then
          return false
        if (Compiler.getKotlinMemberInfo? (← getEnv) fn2).isSome && args2[0]? == some (.fvar x) then
          return false
        return true
      | _ => return false
    else
      match d2.value with
      | .lit _ | .erased | .box .. | .unbox .. | .isShared .. | .reset ..
      | .oproj .. | .sproj .. | .uproj .. =>
        usedBeforeSideEffect x k2
      | .ctor info _ =>
        if !Compiler.isMutableKotlinClass (← getEnv) info.name.getPrefix then
          usedBeforeSideEffect x k2
        else
          return false
      | .fap .. =>
        if ← isPureLet d2 then usedBeforeSideEffect x k2 else return false
      | _ => return false
  | _ => return false

def recordIntSource (n : String) (rhs : String) (decl : LetDecl .impure) : EmitM Unit := do
  if let some intExpr := stripToLong? rhs then
    modify fun st => { st with intSources := st.intSources.insert n intExpr }
  else if let .fap fn args := decl.value then
    if args.size == 1 && (fn == ``Int32.toInt64 || fn == ``ISize.toInt64 || fn == ``UInt32.toUInt64 ||
                          fn == ``Int64.ofInt || fn == ``UInt32.toNat || fn == ``Int32.toNatClampNeg || fn == ``USize.toNat) then
      let a0 ← toKotlinArg args[0]!
      modify fun st => { st with intSources := st.intSources.insert n a0 }

def tryInlineSingleUseLet? (decl : LetDecl .impure) (k : Code .impure) : EmitM Bool := do
  unless ← isInlineableLet decl do return false
  unless countUsesCode decl.fvarId k == 1 do return false
  unless ← usedBeforeSideEffect decl.fvarId k do return false
  let rhs ← emitLetValue decl
  let ty ← inferLetKotlinType decl
  recordVarType rhs ty
  recordIntSource rhs rhs decl
  setParamVarName decl.fvarId rhs
  return true

partial def retYieldsFv (fv : FVarId) (c : Code .impure) : Bool :=
  match skipRC c with
  | .return r => r == fv
  | .let d k =>
    match d.value with
    | .box _ f =>
      if f == fv then retYieldsFv d.fvarId k
      else retYieldsFv fv k
    | .ctor info args =>
      if info.name == ``Prod.mk then
        if args.any (fun a => match a with | .fvar f => f == fv | _ => false) then
          match skipRC k with
          | .return r => r == d.fvarId
          | _ => retYieldsFv fv k
        else retYieldsFv fv k
      else retYieldsFv fv k
    | _ => retYieldsFv fv k
  | _ => false

/--
Detects if `code` tests `fv >= 0` and returns `fv` on success:
`let _x := decLe 0 fv; cases _x | false => contCode | true => return fv`
Returns `some contCode` if matched, where `contCode` does not use `fv`.
-/
partial def isNonNegEarlyExit? (fv : FVarId) (code : Code .impure) : Option (Code .impure) :=
  match skipRC code with
  | .let _ k =>
    if countUsesCode fv k > 0 then
      isNonNegEarlyExit? fv k
    else none
  | .cases cs =>
    if cs.alts.size == 2 then
      let alt0 := cs.alts[0]!
      let alt1 := cs.alts[1]!
      let checkAlt (retAlt contAlt : Alt .impure) : Option (Code .impure) := do
        if countUsesCode fv contAlt.getCode != 0 then none
        else
          if retYieldsFv fv retAlt.getCode then some contAlt.getCode
          else none
      checkAlt alt1 alt0 <|> checkAlt alt0 alt1
    else none
  | _ => none

mutual

partial def emitLetAndContinue (decl : LetDecl .impure) (k : Code .impure) : EmitM Unit := do
  if let some (callee, args) ← loopExpandable? decl.value then
    let curExit ← currentExit
    if let .return r := skipRC k then
      if (r == decl.fvarId || ((← read).retUnit && (curExit matches .funcReturn))) &&
         !(curExit matches .inlineAlias _ | .assignOnly ..) then
        emitLoopExpansion callee args .tail
        return
    if let some contCode := isNonNegEarlyExit? decl.fvarId k then
      if !(curExit matches .inlineAlias _ | .assignOnly ..) then
        emitLoopExpansion callee args (.threaded curExit)
        emitCode contCode
        return
    if let .let d2 k2 := skipRC k then
      if let .unbox f := d2.value then
        if f == decl.fvarId then
          let curExit ← currentExit
          if let .return r := skipRC k2 then
            if (r == d2.fvarId || ((← read).retUnit && (curExit matches .funcReturn))) &&
               !(curExit matches .inlineAlias _ | .assignOnly ..) then
              emitLoopExpansion callee args .tail
              return
          emitLoopExpansion callee args (.assign d2.fvarId d2.type)
          setParamVarName decl.fvarId (← getVarName d2.fvarId)
          emitCode k2
          return
    emitLoopExpansion callee args (.assign decl.fvarId decl.type)
  else if let some (lam, args) ← papBeta? decl.value then
    let curExit ← currentExit
    if let .return r := skipRC k then
      if (r == decl.fvarId || ((← read).retUnit && (curExit matches .funcReturn))) &&
         !(curExit matches .inlineAlias _ | .assignOnly ..) then
        emitLoopExpansion lam args .tail
        return
    if let some contCode := isNonNegEarlyExit? decl.fvarId k then
      if !(curExit matches .inlineAlias _ | .assignOnly ..) then
        emitLoopExpansion lam args (.threaded curExit)
        emitCode contCode
        return
    -- Closures return boxed values; fuse `let y := unbox x` so the inlined body yields `y` directly.
    if let .let d2 k2 := skipRC k then
      if let .unbox f := d2.value then
        if f == decl.fvarId then
          let curExit ← currentExit
          if let .return r := skipRC k2 then
            if (r == d2.fvarId || ((← read).retUnit && (curExit matches .funcReturn))) &&
               !(curExit matches .inlineAlias _ | .assignOnly ..) then
              emitLoopExpansion lam args .tail
              return
          emitLoopExpansion lam args (.assign d2.fvarId d2.type)
          setParamVarName decl.fvarId (← getVarName d2.fvarId)
          emitCode k2
          return
    emitLoopExpansion lam args (.assign decl.fvarId decl.type)
  else if ← recordPap? decl k then
    pure ()
  else if ← tryAliasLet? decl then
    pure ()
  else if ← tryInlineSingleUseLet? decl k then
    pure ()
  else
    let x := decl.fvarId
    match decl.value with
    | .ctor info args =>
      if info.name == ``Prod.mk then
        let strs ← args.mapM toKotlinArg
        if onlyUsedAsVirtualTuple x k then
          let n ← getVarName x
          modify fun st => { st with tuples := st.tuples.insert n strs }
        else
          let (pairExpr, pairTy) ← formatKotlinPair strs none
          if !(← get).aliases.values.contains x && countUsesCode x k == 1 && (← usedBeforeSideEffect x k) then
            setParamVarName x pairExpr
            recordVarType pairExpr pairTy
            modify fun st => {
              st with
              tuples := st.tuples.insert pairExpr strs
              materializedTuples := st.materializedTuples.insert pairExpr
            }
          else
            let n ← getVarName x
            recordVarType n pairTy
            emitLn s!"val {n} = {pairExpr}"
            modify fun st => {
              st with
              tuples := st.tuples.insert n strs
              materializedTuples := st.materializedTuples.insert n
            }
      else if Compiler.isMutableKotlinClass (← getEnv) info.name.getPrefix then
        let s := info.name.getPrefix
        -- Update in place of the value the other fields are read from (see
        -- `Ownership.visitClassCtor?`, which verifies that it is no longer referenced).
        let mut src? : Option FVarId := none
        for a in args ++ (Ownership.ctorScalars x k).map .fvar do
          if let .fvar a := a then
            if let some y := (← get).projSrc[← getVarName a]? then
              if let some prev := src? then
                if (← getVarName prev) != (← getVarName y) then
                  throwError "Kotlin backend: ambiguous in-place update of a `{s}`"
              src? := some y
        if src?.isNone then
          for y in (← get).projSrc.values do
            if (← mutableClassStructOf? y) == some s then
              if let some prev := src? then
                if (← getVarName prev) != (← getVarName y) then
                  throwError "Kotlin backend: ambiguous in-place update of a `{s}`"
              src? := some y
        let some y := src?
          | throwError "Kotlin backend: in `{(← read).currFn}`: cannot allocate a new `{s}` (`@[mutable_kotlin_class]` values are only updated in place)"
        let clsLayout ← classLayout s
        for h : k in [:args.size] do
          if let .fvar v := args[k] then
            if ← isWriteBack x v then continue
          let mut vStr ← toKotlinArg args[k]
          if let some fTy := clsLayout.objTypes[k]? then
            vStr ← castIfNeeded vStr fTy
          emitFieldWrite y (·.objs[k]?) vStr
        setParamVarName x (← getVarName y)
      else if let some spec := Compiler.getKotlinClassSpec? (← getEnv) (match (← getEnv).find? info.name with | some (.ctorInfo cv) => cv.induct | _ => info.name.getPrefix) then
        let env ← getEnv
        let inductName := match env.find? info.name with | some (.ctorInfo cv) => cv.induct | _ => info.name.getPrefix
        let clsName := spec.baseClassName
        let variantName := toPascalCase info.name.getString!
        if Compiler.isKotlinEnum env inductName then
          let rhs := s!"{clsName}.{variantName}"
          if !(← get).aliases.values.contains x && countUsesCode x k == 1 && (← usedBeforeSideEffect x k) then
            setParamVarName x rhs
            recordVarType rhs clsName
          else
            let n ← getVarName x
            recordVarType n clsName
            emitLn s!"val {n} = {rhs}"
          emitCode k
          return
        else if Compiler.isKotlinInductive env inductName then
          let some (.ctorInfo cv) := env.find? info.name | unreachable!
          if cv.numFields == 0 then
            let rhs := s!"{clsName}.{variantName}"
            if !(← get).aliases.values.contains x && countUsesCode x k == 1 && (← usedBeforeSideEffect x k) then
              setParamVarName x rhs
              recordVarType rhs clsName
            else
              let n ← getVarName x
              recordVarType n clsName
              emitLn s!"val {n} = {rhs}"
            emitCode k
            return
          else
            let { objs, usizes, scalars, cont := k' } ← collectCtorFieldSets x (args.mapM toKotlinArg) k
            let layout ← getCtorLayout info.name
            let ctorLayout ← ctorClassLayout info.name
            let mut ctorArgs : Array String := #[]
            for fi in layout.fieldInfo do
              match fi with
              | .object i _ =>
                let mut vStr := objs[i]?.getD "null"
                if let some fTy := ctorLayout.objTypes[i]? then
                  if !ctorLayout.typeParams.any (fTy.contains ·) then
                    vStr ← castIfNeeded vStr fTy
                ctorArgs := ctorArgs.push vStr
              | .usize i => ctorArgs := ctorArgs.push (usizes[i]?.getD "0")
              | .scalar _ off _ => ctorArgs := ctorArgs.push (scalars[off]?.getD "0")
              | _ => pure ()
            let typeArgs :=
              if !ctorLayout.typeParams.isEmpty then
                s!"<{String.intercalate ", " (ctorLayout.typeParams.map fun _ => "Any?").toList}>"
              else ""
            let rhs := s!"{clsName}.{variantName}{typeArgs}({String.intercalate ", " ctorArgs.toList})"
            if !(← get).aliases.values.contains x && countUsesCode x k' == 1 && (← usedBeforeSideEffect x k') then
              setParamVarName x rhs
              recordVarType rhs clsName
            else
              let n ← getVarName x
              recordVarType n clsName
              emitLn s!"val {n} = {rhs}"
            emitCode k'
            return
        else
          let { objs, usizes, scalars, cont := k' } ← collectCtorFieldSets x (args.mapM toKotlinArg) k
          let layout ← getCtorLayout info.name
          let clsLayout ← classLayout inductName
          let mut ctorArgs : Array String := #[]
          for fi in layout.fieldInfo do
            match fi with
            | .object i _ =>
              let mut vStr := objs[i]?.getD "null"
              if let some fTy := clsLayout.objTypes[i]? then
                if !clsLayout.typeParams.any (fTy.contains ·) then
                  vStr ← castIfNeeded vStr fTy
              ctorArgs := ctorArgs.push vStr
            | .usize i => ctorArgs := ctorArgs.push (usizes[i]?.getD "0")
            | .scalar _ off _ => ctorArgs := ctorArgs.push (scalars[off]?.getD "0")
            | _ => pure ()
          let typeArgs :=
            if !clsLayout.typeParams.isEmpty && !clsName.contains '<' then
              s!"<{String.intercalate ", " (clsLayout.typeParams.map fun _ => "Any?").toList}>"
            else ""
          let rhs := s!"{clsName}{typeArgs}({String.intercalate ", " ctorArgs.toList})"
          if !(← get).aliases.values.contains x && countUsesCode x k' == 1 && (← usedBeforeSideEffect x k') then
            setParamVarName x rhs
            recordVarType rhs spec.className
          else
            let n ← getVarName x
            recordVarType n spec.className
            emitLn s!"val {n} = {rhs}"
          emitCode k'
          return
      else
        let { objs, usizes, scalars, cont := k' } ← collectCtorFieldSets x (args.mapM toKotlinArg) k
        let rhs ← formatAdtCtor info objs usizes scalars
        if !(← get).aliases.values.contains x && countUsesCode x k' == 1 && (← usedBeforeSideEffect x k') then
          setParamVarName x rhs
          recordVarType rhs "Array<Any?>"
          modify fun st => { st with adtVars := st.adtVars.insert rhs }
        else
          let n ← getVarName x
          recordVarType n "Array<Any?>"
          modify fun st => { st with adtVars := st.adtVars.insert n }
          emitLn s!"val {n} = {rhs}"
        emitCode k'
        return
    | .oproj i y =>
      match (← get).tuples[← getVarName y]? with
      | some comps => setParamVarName x (comps[i]?.getD "null")
      | none => emitFieldRead x y (·.objs[i]?) decl
    | .sproj _ off y => emitFieldRead x y (·.scalars[off]?) decl
    | .uproj i y => emitFieldRead x y (·.usizes[i]?) decl
    | .isShared y =>
      setParamVarName x (if (← isMutableClassFVar y) then "false" else "true")
    | .reset _ y => setParamVarName x (← getVarName y)
    | .reuse y _ _ args =>
      let layout? ← match ← classStructOf? y with
        | some s => some <$> classLayout s
        | none => pure none
      for h : k in [:args.size] do
        if let .fvar v := args[k] then
          if ← isWriteBack x v then continue
        let mut vStr ← toKotlinArg args[k]
        if let some layout := layout? then
          if let some fTy := layout.objTypes[k]? then
            vStr ← castIfNeeded vStr fTy
        emitFieldWrite y (·.objs[k]?) vStr
      setParamVarName x (← getVarName y)
    | .fap fn args =>
      if let some (a, idx, v, checkBounds) ← arraySet? fn args then
        let arrTy ← arrayType a
        let vStr ← toKotlinArg v
        let isByteArraySet :=
          let baseFn := match fn with | .str p "_boxed" => p | _ => fn
          baseFn.getPrefix == `ByteArray
        let vStr ←
          if isByteArraySet then
            pure s!"({← castIfNeeded vStr "UByte"}).toByte()"
          else
            match kotlinArrayElem? arrTy with
            | some e => castIfNeeded vStr e
            | none => pure vStr
        let aStr ← castIfNeeded (← toKotlinArg a) arrTy
        if checkBounds then
          emitLn s!"if ({idx} >= 0 && {idx} < {aStr}.size) {aStr}[{idx}] = {vStr}"
        else
          emitLn s!"{aStr}[{idx}] = {vStr}"
        setParamVarName x aStr
      else if let some (a, iStr, jStr, checkBounds) ← arraySwap? fn args then
        let arrTy ← arrayType a
        let aStr ← castIfNeeded (← toKotlinArg a) arrTy
        let count := (← get).nameCounter + 1
        modify fun st => { st with nameCounter := count }
        let tmpName := s!"tmp_{count}"
        if checkBounds then
          emitLn s!"if ({iStr} >= 0 && {iStr} < {aStr}.size && {jStr} >= 0 && {jStr} < {aStr}.size) \{ val {tmpName} = {aStr}[{iStr}]; {aStr}[{iStr}] = {aStr}[{jStr}]; {aStr}[{jStr}] = {tmpName} }"
        else
          emitLn s!"val {tmpName} = {aStr}[{iStr}]"
          emitLn s!"{aStr}[{iStr}] = {aStr}[{jStr}]"
          emitLn s!"{aStr}[{jStr}] = {tmpName}"
        setParamVarName x aStr
      else if let some ai := Ownership.inplaceArg? (← getEnv) fn args then
        -- `kotlin_inplace:` extern: a statement updating argument `ai`, which is the result.
        emitLn (← emitLetValue decl)
        setParamVarName x (← toKotlinArg args[ai]!)
      else
        -- A callee that updates values in place may write fields.
        if let some s := (← read).summaries[fn]? then
          if s.exclusive.any id then forgetFieldVals
        if let some shape ← dropShape? fn then
          let call ← emitLetValue decl
          if isUnitShape shape then
            emitLn call
            registerDropped x shape args "null"
          else
            let n ← getVarName x
            let keptTy ← fnRetKotlinType fn decl.type
            recordVarType n keptTy
            emitLn s!"val {n} = {call}"
            registerDropped x shape args n
        else
          let baseFn := match fn with | .str p "_boxed" => p | _ => fn
          if !(← isUsed x) && (baseFn == `ST.Prim.Ref.swap || baseFn == `ST.Prim.Ref.swap._redArg) && args.size >= 3 then
            let r ← castIfNeeded (← toKotlinArg args[args.size - 3]!) "Array<Any?>"
            let a ← toKotlinArg args[args.size - 2]!
            emitLn s!"{r}[0] = {a}"
          else
            let rhs ← emitLetValue decl
            let ty ← inferLetKotlinType decl
            let isAdtRes ← match (← getMonoDecl? fn).bind monoResultType? with
              | some r => pure (isAdtMonoType r)
              | none => pure false
            if !(← isUsed x) then
              if ty == "Unit" then
                let stmt := if rhs.startsWith "run { " && rhs.endsWith " }" then
                  ((rhs.drop 6).take (rhs.length - 8)).toString
                else rhs
                emitLn stmt
              else if ← isPureConstantLet decl then
                pure ()
              else
                let n ← getVarName x
                recordVarType n ty
                if isAdtRes then modify fun st => { st with adtVars := st.adtVars.insert n }
                recordIntSource n rhs decl
                emitLn s!"val {n} = {rhs}"
            else
              let n ← getVarName x
              recordVarType n ty
              if isAdtRes then modify fun st => { st with adtVars := st.adtVars.insert n }
              recordIntSource n rhs decl
              emitLn s!"val {n} = {rhs}"
    | _ =>
      if !(← isUsed x) && (← isPureConstantLet decl) then
        pure ()
      else
        let n ← getVarName x
        let rhs ← emitLetValue decl
        let ty ← inferLetKotlinType decl
        recordVarType n ty
        recordIntSource n rhs decl
        emitLn s!"val {n} = {rhs}"
  emitCode k

partial def emitCode (code : Code .impure) : EmitM Unit := do
  match code with
  | .let decl k =>
    match skipRC k with
    | .return fvarId =>
      let curExit ← currentExit
      let isTailTarget := fvarId == decl.fvarId || ((← read).retUnit && (curExit matches .funcReturn))
      if isTailTarget && !(curExit matches .inlineAlias _ | .assignOnly ..) then
        if let some l := (← read).loop? then
          if let .fap fn args := decl.value then
            if fn == l.fnName then
              emitLoopContinue l args
              return
        if let some (callee, args) ← loopExpandable? decl.value then
          emitLoopExpansion callee args .tail
          return
        if let some (lam, args) ← papBeta? decl.value then
          emitLoopExpansion lam args .tail
          return
        if fvarId == decl.fvarId then
          -- Self tail call whose dropped results are passed through unchanged: keep it a tail call.
          if let .fap fn args := decl.value then
            if let some shape := (← read).retShape? then
              if fn == (← read).currFn && (← read).loop?.isNone then
                let ps := (← read).currParams
                let ok ← shape.identities.allM fun (_, j) => do
                  match ps[j]? with
                  | some p => return (← toKotlinArg (args[j]?.getD .erased)) == (← getVarName p.fvarId)
                  | none => return false
                if ok then
                  emitReturn (← emitLetValue decl) (some (← inferLetKotlinType decl))
                  return
          let needsShape ← match decl.value with
            | .fap fn _ => do
              pure ((← dropShape? fn).isSome || Ownership.isArraySet fn || Ownership.isScalarArraySet fn ||
                Ownership.isInplaceExtern (← getEnv) fn)
            | .ctor .. => pure true
            | .oproj _ y => do pure ((← get).tuples.contains (← getVarName y))
            | .reset .. | .reuse .. => pure true
            | _ => pure false
          if needsShape || (← read).retShape?.isSome then
            emitLetAndContinue decl k
            return
          if ← tryAliasLet? decl then
            emitReturnVar fvarId
            return
          let valStr ← emitLetValue decl
          let valTy ← inferLetKotlinType decl
          emitReturn valStr (some valTy)
        else
          emitLetAndContinue decl k
      else
        emitLetAndContinue decl k
    | _ => emitLetAndContinue decl k
  | .cases cs =>
    let discrName ← getVarName cs.discr
    let isBool := cs.typeName == ``Bool || (← get).varTypes[discrName]? == some "Boolean" ||
      (← get).knownBools.contains discrName
    if isBool && cs.alts.size == 2 then
      let (thenCode, thenWhen) := match cs.alts[0]! with
        | .ctorAlt info c => (c, info.cidx == 1)
        | .default c => (c, true)
      let elseCode := cs.alts[1]!.getCode
      match (← get).knownBools[discrName]? with
      | some b =>
        -- The discriminant was already tested by an enclosing branch.
        emitCode (if b == thenWhen then thenCode else elseCode)
      | none =>
        let thenBuf ← captureBuf <| withFieldVals <| withIndent (withKnown discrName thenWhen (emitCode thenCode))
        let elseBuf ← captureBuf <| withFieldVals <| withIndent (withKnown discrName (!thenWhen) (emitCode elseCode))
        if thenBuf.isEmpty && elseBuf.isEmpty then
          pure ()
        else if thenBuf.isEmpty then
          let invCond := if !thenWhen then discrName else s!"!{discrName}"
          emitIndent; emit s!"if ({invCond}) "; emitLn "{"
          emit elseBuf
          emitLn "}"
        else if elseBuf.isEmpty then
          let cond := if thenWhen then discrName else s!"!{discrName}"
          emitIndent; emit s!"if ({cond}) "; emitLn "{"
          emit thenBuf
          emitLn "}"
        else
          let cond := if thenWhen then discrName else s!"!{discrName}"
          emitIndent; emit s!"if ({cond}) "; emitLn "{"
          emit thenBuf
          emitIndent; emit "} else "; emitLn "{"
          emit elseBuf
          emitLn "}"
    else
      if cs.typeName == `obj || cs.typeName == `tobj then
        modify fun st => { st with adtVars := st.adtVars.insert discrName }
      if cs.alts.isEmpty then
        emitLn "error(\"unreachable\")"
      else
        let env ← getEnv
        let discrTy? := (← get).varTypes[discrName]?
        let inductFromAlts? : Option Name :=
          cs.alts.findSome? fun
            | .ctorAlt info _ =>
              match env.find? info.name with
              | some (.ctorInfo cv) => some cv.induct
              | _ => some info.name.getPrefix
            | _ => none
        let discrInduct? := inductFromAlts?.filter (Compiler.isKotlinInductive env ·)
        if cs.alts.size == 1 then
          match cs.alts[0]! with
          | .ctorAlt info altCode =>
            if discrInduct?.isSome then
              let savedStructs := (← get).nameStructs
              modify fun st => { st with nameStructs := st.nameStructs.insert discrName info.name }
              withFieldVals (emitCode altCode)
              modify fun st => { st with nameStructs := savedStructs }
            else
              withFieldVals (emitCode altCode)
          | .default altCode =>
            withFieldVals (emitCode altCode)
        else if let some inductName := discrInduct?.filter (fun n =>
          if Compiler.isKotlinEnum env n then
            discrTy? != some "UByte" && discrTy? != some "UShort" && discrTy? != some "UInt"
          else true) then
          let spec := (Compiler.getKotlinClassSpec? env inductName).get!
          let cls := spec.baseClassName
          let isEnum := Compiler.isKotlinEnum env inductName
          let typeParams ← (Meta.MetaM.run' (getDeclTypeParams env inductName) : CoreM (Array (Name × String)))
          let starProj := if !typeParams.isEmpty then "<*>" else ""
          emitIndent; emit s!"when ({discrName}) "; emitLn "{"
          withIndent do
            for alt in cs.alts do
              match alt with
              | .ctorAlt info altCode =>
                let variantName := toPascalCase info.name.getString!
                let cond :=
                  if isEnum then
                    s!"{cls}.{variantName}"
                  else
                    match env.find? info.name with
                    | some (.ctorInfo cv) =>
                      if cv.numFields == 0 then s!"{cls}.{variantName}"
                      else s!"is {cls}.{variantName}{starProj}"
                    | _ => s!"is {cls}.{variantName}{starProj}"
                emitIndent; emit s!"{cond} -> "; emitLn "{"
                let savedStructs := (← get).nameStructs
                modify fun st => { st with nameStructs := st.nameStructs.insert discrName info.name }
                withFieldVals <| withIndent (emitCode altCode)
                modify fun st => { st with nameStructs := savedStructs }
                emitLn "}"
              | .default altCode =>
                emitIndent; emit "else -> "; emitLn "{"
                withFieldVals <| withIndent (emitCode altCode)
                emitLn "}"
            unless cs.alts.any (· matches .default _) do
              let some iv := isInductiveCore? env inductName | pure ()
              if cs.alts.size < iv.ctors.length || discrTy? != some cls then
                emitIndent; emit "else -> "; emitLn "{ error(\"unreachable\") }"
          emitLn "}"
        else
          let discrExpr ←
            if cs.typeName == `obj || cs.typeName == `tobj || discrTy? == some "Array<Any?>" then
              if discrTy? == some "Array<Any?>" then
                pure s!"({discrName}[0] as Int)"
              else
                pure s!"(({discrName} as Array<*>)[0] as Int)"
            else if discrTy? == some "UByte" || discrTy? == some "UShort" || discrTy? == some "UInt" then
              pure s!"({discrName}).toInt()"
            else
              pure discrName
          emitIndent; emit s!"when ({discrExpr}) "; emitLn "{"
          withIndent do
            for alt in cs.alts do
              match alt with
              | .ctorAlt info altCode =>
                emitIndent; emit s!"{info.cidx} -> "; emitLn "{"
                withFieldVals <| withIndent (emitCode altCode)
                emitLn "}"
              | .default altCode =>
                emitIndent; emit "else -> "; emitLn "{"
                withFieldVals <| withIndent (emitCode altCode)
                emitLn "}"
            unless cs.alts.any (· matches .default _) do
              emitIndent; emit "else -> "; emitLn "{ error(\"unreachable\") }"
          emitLn "}"
  | .return fvarId =>
    emitReturnVar fvarId
  | .oset x i y k =>
    emitFieldWrite x (·.objs[i]?) (← toKotlinArg y)
    emitCode k
  | .sset x _ off y _ k =>
    unless ← isWriteBack x y do emitFieldWrite x (·.scalars[off]?) (← getVarName y)
    emitCode k
  | .uset x i y k =>
    unless ← isWriteBack x y do emitFieldWrite x (·.usizes[i]?) (← getVarName y)
    emitCode k
  | .jp decl k =>
    let useCount := countJmp decl.fvarId k
    let isRec := countJmp decl.fvarId decl.value > 0
    if useCount == 0 then
      emitCode k
    else if useCount == 1 && !isRec then
      modify fun st => { st with inlinedJps := st.inlinedJps.insert decl.fvarId decl }
      emitCode k
    else if !isRec then
      let params := decl.params
      let aliases := (← get).aliases
      let mut isAliased : Array Bool := #[]
      let useLbl := needsJpLabel decl.fvarId k
      let lbl? ← if useLbl then
        let c := (← get).loopCounter + 1
        modify fun st => { st with loopCounter := c }
        pure (some s!"jp_{c}")
      else
        pure none
      for p in params do
        if aliases.contains p.fvarId then
          isAliased := isAliased.push true
        else
          let pName ← getVarName p.fvarId
          let pType := toKotlinType p.type
          recordVarType pName pType
          emitLn s!"val {pName}: {pType}"
          isAliased := isAliased.push false
      modify fun st => { st with blockJps := st.blockJps.insert decl.fvarId (decl, lbl?, isAliased) }
      match lbl? with
      | some lbl =>
        emitIndent; emit s!"{lbl}@ do "; emitLn "{"
        withIndent (emitCode k)
        emitIndent; emitLn "} while (false)"
      | none =>
        emitCode k
      forgetFieldVals
      emitCode decl.value
    else
      let jpName ← getVarName decl.fvarId
      let params := decl.params
      let ctx ← read
      let shape? := ctx.retShape?
      let unitShape := shape?.any isUnitShape
      let retType := if unitShape then "Unit" else ctx.retCast?.getD (toKotlinType decl.type)
      recordVarType jpName retType
      let paramDecls : Array String ← params.mapM fun p => do
        let pName ← getVarName p.fvarId
        let pType := toKotlinType p.type
        recordVarType pName pType
        return s!"{pName}: {pType}"
      let paramStr := String.intercalate ", " paramDecls.toList
      emitIndent; emit s!"fun {jpName}({paramStr}): {retType} "; emitLn "{"
      -- A local function body returns from itself, not from any enclosing loop block.
      withFieldVals do
        forgetFieldVals
        withIndent (withReader (fun ctx => { ctx with loop? := none, retCast? := none, retUnit := unitShape }) (emitCode decl.value))
      emitLn "}"
      emitCode k
  | .jmp fvarId args =>
    if let some decl := (← get).inlinedJps[fvarId]? then
      for (p, arg) in decl.params.zip args do
        let argStr ← toKotlinArg arg
        setParamVarName p.fvarId argStr
      emitCode decl.value
    else if let some (decl, lbl?, isAliased) := (← get).blockJps[fvarId]? then
      for i in [:decl.params.size] do
        unless isAliased[i]! do
          let p := decl.params[i]!
          let pName ← getVarName p.fvarId
          let pType := toKotlinType p.type
          let aStr ← toKotlinArg (args[i]?.getD .erased)
          let aCast ← castIfNeeded aStr pType
          emitLn s!"{pName} = {aCast}"
      if let some lbl := lbl? then
        emitLn s!"break@{lbl}"
    else
      let fnName ← getVarName fvarId
      let argStrs ← args.mapM toKotlinArg
      let argStr := String.intercalate ", " argStrs.toList
      emitReturn s!"{fnName}({argStr})" (← get).varTypes[fnName]?
  | .unreach _ =>
    emitLn "error(\"unreachable\")"
  | c =>
    if let some k := skipCont? c then emitCode k

/--
Emits the body of the loop-expandable `callee` applied to `args` as a `while (true)` loop.
`dest? = none`: tail position, results go wherever the enclosing context returns to.
`dest? = some (x, ty)`: assigns result to `x`.
-/
partial def emitLoopExpansion (callee : Decl .impure) (args : Array (Arg .impure))
    (dest : LoopDest) : EmitM Unit := do
  let callee ← callee.internalize (uniqueIdents := true)
  let .code body := callee.value | return
  let body ← simplifyResetReuse body
  markUsed body
  let n := (← get).loopCounter + 1
  modify fun st => { st with loopCounter := n }
  let params := callee.params
  let selfArgs := collectSelfCallArgs callee.name body #[]
  let aliases ← collectAliases params body
  modify fun st => { st with aliases := aliases.fold (init := st.aliases) fun m k v => m.insert k v }
  let root (f : FVarId) : FVarId := aliases.getD f f
  let variant := params.mapIdx fun i p => selfArgs.any fun as =>
    match as[i]? with
    | some (.fvar f) => root f != p.fvarId
    | _ => true
  let loopLbl := s!"loop_{n}"
  let isLoop := hasSelfCall callee.name body
  let emitParams : EmitM (Array String) := do
    let mut names : Array String := #[]
    for i in [:params.size] do
      let p := params[i]!
      let a ← toKotlinArg (args[i]?.getD .erased)
      let pTy := toKotlinType p.type
      if variant[i]! then
        let nm ← getVarName p.fvarId
        recordVarType nm pTy
        let aCast ← castIfNeeded a pTy
        emitLn s!"var {nm}: {pTy} = {aCast}"
        names := names.push nm
      else
        let isSimple := a.all (fun c => c.isAlphanum || c == '_') || a == "true" || a == "false" || a == "null"
        if isSimple then
          -- Loop-invariant parameter: alias the argument, no copy and no per-iteration update.
          setParamVarName p.fvarId a
          if ((← get).varTypes[a]?.getD "Any?") == "Any?" && pTy != "Any?" then
            recordVarType a pTy
          names := names.push a
        else
          let nm ← getVarName p.fvarId
          recordVarType nm pTy
          let aCast ← castIfNeeded a pTy
          emitLn s!"val {nm}: {pTy} = {aCast}"
          setParamVarName p.fvarId nm
          names := names.push nm
    return names
  match dest with
  | .tail =>
    let outerExit ← currentExit
    let names ← emitParams
    let lc : LoopCtx := { fnName := callee.name, varNames := names, variant, loopLbl, exit := outerExit }
    if isLoop then
      forgetFieldVals
      emitIndent; emit s!"{loopLbl}@ while (true) "; emitLn "{"
      withIndent (withReader (fun ctx => { ctx with loop? := some lc }) (emitCode body))
      emitLn "}"
    else
      withReader (fun ctx => { ctx with loop? := some lc }) (emitCode body)
  | .threaded outerExit =>
    let names ← emitParams
    let lc : LoopCtx := { fnName := callee.name, varNames := names, variant, loopLbl, exit := .threaded outerExit loopLbl }
    if isLoop then
      forgetFieldVals
      emitIndent; emit s!"{loopLbl}@ while (true) "; emitLn "{"
      withIndent (withReader (fun ctx => { ctx with loop? := some lc }) (emitCode body))
      emitLn "}"
    else
      withReader (fun ctx => { ctx with loop? := some lc }) (emitCode body)
    forgetFieldVals
  | .assign fv ty =>
    let shape? ← dropShape? callee.name
    let unitShape := shape?.any isUnitShape
    if unitShape then
      let names ← emitParams
      if isLoop then
        forgetFieldVals
        let lc : LoopCtx := { fnName := callee.name, varNames := names, variant, loopLbl, exit := .breakOnly loopLbl }
        emitIndent; emit s!"{loopLbl}@ while (true) "; emitLn "{"
        withIndent (withReader (fun ctx => { ctx with loop? := some lc, retShape? := shape? }) (emitCode body))
        emitLn "}"
        forgetFieldVals
      else if !hasBranches body then
        let lc : LoopCtx := { fnName := callee.name, varNames := names, variant, loopLbl, exit := .inlineAlias fv }
        withReader (fun ctx => { ctx with loop? := some lc, retShape? := shape? }) (emitCode body)
      else
        let lc : LoopCtx := { fnName := callee.name, varNames := names, variant, loopLbl, exit := .breakOnly loopLbl }
        emitIndent; emit s!"{loopLbl}@ do "; emitLn "{"
        withIndent (withReader (fun ctx => { ctx with loop? := some lc, retShape? := shape? }) (emitCode body))
        emitIndent; emitLn "} while (false)"
        forgetFieldVals
      if let some shape := shape? then
        registerDropped fv shape args "null"
    else if !isLoop && !hasBranches body && shape?.isNone then
      let names ← emitParams
      let lc : LoopCtx := { fnName := callee.name, varNames := names, variant, loopLbl, exit := .inlineAlias fv }
      withReader (fun ctx => { ctx with loop? := some lc, retShape? := none }) (emitCode body)
    else
      let resTy ← match shape? with
        | some _ => fnRetKotlinType callee.name ty
        | none => pure (toKotlinType ty)
      let x ← getVarName fv
      recordVarType x resTy
      let names ← emitParams
      if isLoop then
        forgetFieldVals
        modify fun st => { st with loopExits := st.loopExits.insert loopLbl #[] }
        let lc : LoopCtx := { fnName := callee.name, varNames := names, variant, loopLbl, exit := .assignAndBreak x resTy loopLbl }
        let loopBuf ← captureBuf do
          emitIndent; emit s!"{loopLbl}@ while (true) "; emitLn "{"
          withIndent (withReader (fun ctx => { ctx with loop? := some lc, retShape? := shape? }) (emitCode body))
          emitLn "}"
        forgetFieldVals
        let exits := (← get).loopExits.getD loopLbl #[]
        if shape?.isNone && !exits.isEmpty && (exits.all (· == "true") || exits.all (· == "false")) then
          let constVal := exits[0]!
          setParamVarName fv constVal
          modify fun st => { st with knownBools := st.knownBools.insert x (constVal == "true") }
          let assignLine := s!"{x} = {constVal}"
          let filtered := String.intercalate "\n" ((splitLines loopBuf).filter fun l =>
            String.ofList (l.toList.dropWhile (· == ' ')) != assignLine)
          emit filtered
        else
          emitLn s!"val {x}: {resTy}"
          emit loopBuf
      else if !hasMultiUseJP body then
        emitLn s!"val {x}: {resTy}"
        let lc : LoopCtx := { fnName := callee.name, varNames := names, variant, loopLbl, exit := .assignOnly x resTy }
        withReader (fun ctx => { ctx with loop? := some lc, retShape? := shape? }) (emitCode body)
        forgetFieldVals
      else
        emitLn s!"val {x}: {resTy}"
        let lc : LoopCtx := { fnName := callee.name, varNames := names, variant, loopLbl, exit := .assignAndBreak x resTy loopLbl }
        emitIndent; emit s!"{loopLbl}@ do "; emitLn "{"
        withIndent (withReader (fun ctx => { ctx with loop? := some lc, retShape? := shape? }) (emitCode body))
        emitIndent; emitLn "} while (false)"
        forgetFieldVals
      if let some shape := shape? then
        registerDropped fv shape args x

end

/-- KDoc lines for a Lean docstring (blank leading/trailing lines dropped). -/
def kdocLines (doc : String) : List String :=
  let isBlank (l : String) := l.all (· == ' ')
  let lines := (splitLines doc).dropWhile isBlank |>.reverse |>.dropWhile isBlank |>.reverse
  if lines.isEmpty then []
  else ["/**"] ++ lines.map (fun l => if isBlank l then " *" else " * " ++ l) ++ [" */"]

def emitFnDecl (decl : Decl .impure) : EmitM Unit := do
  let origParamNames := decl.params.map (·.binderName)
  let decl ← decl.internalize (uniqueIdents := true)
  let initKnownBools := ({} : Std.HashMap String Bool).insert "true" true |>.insert "false" false
  let initVarTypes := ({} : Std.HashMap String String).insert "true" "Boolean" |>.insert "false" "Boolean" |>.insert "Unit" "Unit"
  modify fun st => { st with varNames := {}, nameCounter := 0, inlinedJps := {}, blockJps := {}, knownBools := initKnownBools, loopCounter := 0, paps := {}, tuples := {}, materializedTuples := {}, nameStructs := {}, fieldVals := {}, used := {}, projSrc := {}, writeBacks := {}, varTypes := initVarTypes, aliases := {}, loopExits := {}, adtVars := {} }
  let .code code := decl.value | return ()
  let code ← simplifyResetReuse code
  markUsed code
  let fnAliases ← collectAliases decl.params code
  modify fun st => { st with aliases := fnAliases }
  let env ← getEnv
  let opts ← getOptions
  let member? := Compiler.getKotlinMemberInfo? env decl.name
  let params := decl.params
  -- `@[kotlin_types]`: emitted parameter types followed by the return type; `"_"` = default.
  let tyOverrides := (Compiler.getKotlinTypes? env decl.name).getD #[]
  let override? (i : Nat) : Option String :=
    match tyOverrides[i]? with
    | some t => if t == "_" then none else some t
    | none => none
  let isAux := isCompilerAuxDecl env decl.name
  let numFnTypeParams ← (Meta.MetaM.run' do
    if isAux then return 0
    getDeclNumTypeParams env decl.name : CoreM Nat)
  let runtimeParams := params.extract numFnTypeParams params.size
  let numEmitted := if member?.isSome then runtimeParams.size - 1 else runtimeParams.size
  let retCast? := override? numEmitted
  -- A Lean result type of `Unit` becomes a Kotlin `Unit` return type.
  let rec resultType : Expr → Expr
    | .forallE _ _ b _ => resultType b
    | e => e
  let retUnit := retCast?.isNone && match env.find? decl.name with
    | some ci => resultType ci.type == mkConst ``Unit
    | none => false
  -- Results identical to parameters are dropped (see `dropShape?`).
  let shape? ← if decl.type.isScalar then pure none else dropShape? decl.name
  let (retUnit, retCast?) ← match shape? with
    | some shape =>
      if isUnitShape shape then pure (true, none)
      else match retCast? with
        | some t => pure (false, some t)
        | none =>
          let c := (keptComps shape)[0]!
          let t? ← match env.find? decl.name with
            | some ci => prodComponentKotlinType? (resultType ci.type) c
            | none => pure none
          pure (false, t?)
    | none => pure (retUnit, retCast?)
  let monoDecl? ← getMonoDecl? decl.name
  let fnTypeInfo? ← (Meta.MetaM.run' do
    if isAux then return none
    let some info := env.find? decl.name | return none
    let typeParams ← getDeclTypeParams env decl.name
    let typeParamMap := typeParams.foldl (init := ({} : Std.HashMap Name String)) fun m (n, t) => m.insert n t
    Meta.forallTelescopeReducing info.type fun xs retType => do
      let runtimeXs ← xs.filterM fun x => do return !(← Meta.inferType x).isSort
      let paramTys ← runtimeXs.mapM fun x => do formatGenericKotlinType env typeParamMap (← Meta.inferType x)
      let retTy ← formatGenericKotlinType env typeParamMap retType
      return some (typeParams, paramTys, retTy) : CoreM (Option (Array (Name × String) × Array String × String)))
  let classTypeParams ← match member? with
    | some info =>
      match (← read).classStructs[info.className]? with
      | some s => (Meta.MetaM.run' (getDeclTypeParams env s) : CoreM (Array (Name × String)))
      | none => pure #[]
    | none => pure #[]
  let classTypeParamNames := classTypeParams.map (·.2)
  let allFnTypeParams := (fnTypeInfo?.map (·.1)).getD #[]
  let funcTypeParams :=
    if member?.isSome then
      allFnTypeParams.filter fun (_, kt) => !classTypeParamNames.contains kt
    else
      allFnTypeParams
  let typeParamStr := if funcTypeParams.isEmpty then "" else s!"<{String.intercalate ", " (funcTypeParams.map (·.2)).toList}> "
  let genericRetTy? := fnTypeInfo?.map (·.2.2)
  let defaultRetTy :=
    if let some gr := genericRetTy? then
      if gr != "Any?" then gr else match monoDecl?.bind monoResultType? with
        | some r => if isRichMonoType r then toKotlinType r else toKotlinType decl.type
        | none => toKotlinType decl.type
    else match monoDecl?.bind monoResultType? with
      | some r => if isRichMonoType r then toKotlinType r else toKotlinType decl.type
      | none => toKotlinType decl.type
  let retType := if retUnit then "Unit" else retCast?.getD defaultRetTy
  let mut paramDecls : Array String := #[]
  let mut seenParamNames : Std.HashSet String := {}
  for i in [:numFnTypeParams] do
    setParamVarName params[i]!.fvarId ""
  for i in [:runtimeParams.size] do
    let p := runtimeParams[i]!
    let fullIdx := numFnTypeParams + i
    if i == 0 && member?.isSome then
      setParamVarName p.fvarId "this"
      if let some info := member? then
        recordVarType "this" info.recvType
        if let some s := (← read).classStructs[info.className]? then
          modify fun st => { st with nameStructs := st.nameStructs.insert "this" s }
    else
      -- Parameter names are part of the Kotlin API (named arguments, API files), so keep the
      -- binder name when it is a plain identifier. Locals always get a numeric suffix.
      let raw := (origParamNames[fullIdx]?.getD p.binderName).eraseMacroScopes.toString
      let pName :=
        if !raw.isEmpty && raw.all (fun c => c.isAlphanum || c == '_') && !(raw.front.isDigit) &&
           raw != "_" && !isKotlinKeyword raw && !seenParamNames.contains raw then raw
        else s!"p_{i}"
      seenParamNames := seenParamNames.insert pName
      setParamVarName p.fvarId pName
      let emittedIdx := if member?.isSome then i - 1 else i
      let genericParamTy? := fnTypeInfo?.bind (·.2.1[i]?)
      let defaultParamTy :=
        if let some gp := genericParamTy? then
          if gp != "Any?" then gp else match monoDecl?.bind (·.params[fullIdx]?) with
            | some mp => if isRichMonoType mp.type then toKotlinType mp.type else toKotlinType p.type
            | none => toKotlinType p.type
        else match monoDecl?.bind (·.params[fullIdx]?) with
          | some mp => if isRichMonoType mp.type then toKotlinType mp.type else toKotlinType p.type
          | none => toKotlinType p.type
      let rawPType := (override? emittedIdx).getD defaultParamTy
      let (paramAnno, pType) :=
        if rawPType.startsWith "@" then
          let chars := rawPType.toList
          match chars.reverse.findIdx? (· == ')') with
          | some revIdx =>
            let idx := chars.length - 1 - revIdx
            let annos := String.ofList (chars.take (idx + 1))
            let ty := (String.ofList (chars.drop (idx + 1))).trimAscii.toString
            (annos ++ " ", ty)
          | none => ("", rawPType)
        else ("", rawPType)
      recordVarType pName pType
      if pType == "Any?" then
        if let some mp := monoDecl?.bind (·.params[fullIdx]?) then
          if isAdtMonoType mp.type then
            modify fun st => { st with adtVars := st.adtVars.insert pName }
      paramDecls := paramDecls.push s!"{paramAnno}{pName}: {pType}"
  let paramStr := String.intercalate ", " paramDecls.toList
  let codeJP := hasRecursiveJP code
  let isTailRec := !codeJP && hasSelfTailCall decl.name code
  let extDeclNames := (← read).extDeclNames
  let isExt := extDeclNames.contains decl.name
  let callsExt := (collectCalls env (← read).declMap code #[]).any (extDeclNames.contains ·)
  let isInline := !codeJP && !hasSelfCall decl.name code && !callsExt && Compiler.hasInlineAttribute env decl.name
  let (mods, fnName) := match member? with
    | some info => (info.modifiers, memberKotlinName decl.name info)
    | none =>
      let m := if isExt then "private" else compiler.kotlin.topLevelModifiers.get opts
      (m, toKotlinFnName decl.name)
  let explicitInline := (identTokens mods).contains "inline"
  let auto :=
    if isTailRec then "tailrec "
    else if isInline && !explicitInline then "inline "
    else ""
  let modsStr := if mods.isEmpty then "" else mods ++ " "
  if let some (.inl doc) ← findInternalDocString? env decl.name (includeBuiltin := false) then
    for l in kdocLines doc do emitLn l
  emitIndent; emit s!"{modsStr}{auto}fun {typeParamStr}{fnName}({paramStr}): {retType} "; emitLn "{"
  withIndent do
    withReader (fun ctx => { ctx with currFn := decl.name, currParams := params, currClass? := member?.map (·.className), retCast? := if retUnit then none else some retType, retUnit, retShape? := shape? }) do
      for p in params do discard <| classStructOf? p.fvarId
      emitCode code
  emitLn "}"
  emitLn ""

/-- Replaces each `// @LeanMembers(Cls)` line with the members of `Cls`, at the marker's indent. -/
def spliceMembers (text : String) (members : Std.HashMap String String) : String × Array String := Id.run do
  let pre := "// @LeanMembers("
  let mut out := ""
  let mut used : Array String := #[]
  for line in splitLines text do
    let trimmedChars := line.toList.dropWhile (· == ' ')
    let trimmed := String.ofList trimmedChars
    if trimmed.startsWith pre then
      let indentStr := String.ofList (List.replicate (line.length - trimmedChars.length) ' ')
      let cls := String.ofList ((trimmedChars.drop pre.length).takeWhile (· != ')'))
      used := used.push cls
      for ml in splitLines (members.getD cls "") do
        if !ml.isEmpty then
          out := out ++ indentStr ++ ml
        out := out ++ "\n"
    else
      out := out ++ line ++ "\n"
  return (out, used)

unsafe def evalFileSpecUnsafe (n : Name) : CoreM _root_.Lean.Compiler.Kotlin.FileSpec := do
  match (← getEnv).evalConst _root_.Lean.Compiler.Kotlin.FileSpec (← getOptions) n (checkMeta := false) with
  | .ok v => return v
  | .error e => throwError "Kotlin backend: cannot evaluate `@[kotlin_file]` constant `{n}`: {e}"

/-- Evaluates a `@[kotlin_file]` constant. -/
@[implemented_by evalFileSpecUnsafe]
opaque evalFileSpec (n : Name) : CoreM _root_.Lean.Compiler.Kotlin.FileSpec

/-- The module's `@[kotlin_file]` constant, if any. -/
def findFileSpecDecl? (env : Environment) : CoreM (Option Name) := do
  let tagged := env.constants.map₂.foldl (init := #[]) fun acc n _ =>
    if Compiler.kotlinFileAttr.hasTag env n then acc.push n else acc
  if tagged.size > 1 then
    throwError "Kotlin backend: more than one `@[kotlin_file]` constant: {tagged}"
  let some n := tagged[0]? | return none
  let expected := mkConst ``_root_.Lean.Compiler.Kotlin.FileSpec
  unless (← getConstInfo n).type == expected do
    throwError "Kotlin backend: `@[kotlin_file]` constant `{n}` must have type `{expected}`"
  return some n

/-- Drops blank lines at both ends of `text`. -/
def trimBlankLines (text : String) : List String :=
  let isBlank (l : String) := l.all (· == ' ')
  (splitLines text).dropWhile isBlank |>.reverse |>.dropWhile isBlank |>.reverse

/-- Prefixes each non-blank line of `text` with `ind`; the result ends with a newline. -/
def reindent (ind : String) (text : String) : String :=
  String.join <| (trimBlankLines text).map fun l =>
    (if l.all (· == ' ') then "" else ind ++ l) ++ "\n"

def fileSpecTexts (spec : _root_.Lean.Compiler.Kotlin.FileSpec) : Array String := Id.run do
  let mut out := #[spec.preamble] ++ spec.fileAnnotations ++ spec.imports
  for item in spec.items do
    match item with
    | .cls c =>
      out := out.push c.header
      for i in c.body do
        match i with
        | .field d | .init d | .verbatim d => out := out.push d
        | .members => pure ()
    | .verbatim code => out := out.push code
    | .topLevel => pure ()
  return out

def isBuiltinArrayFn (fn : Name) : Bool :=
  let fn := match fn with | .str p "_boxed" => p | _ => fn
  (fn.getPrefix == `Array && match fn with
    | .str _ s =>
      s == "get!Internal" || s == "get!InternalBorrowed" ||
      s == "getInternal" || s == "getInternalBorrowed" ||
      s == "uget" || s == "ugetBorrowed" ||
      s == "size" || s == "usize" ||
      s == "replicate" || s == "mkArray" ||
      s == "mkEmpty" || s == "emptyWithCapacity" ||
      s == "toList" || s == "mk" ||
      s == "push" || s == "pop" ||
      s == "append" || s == "appendCore" ||
      s == "extract" ||
      s == "propagateMark" || s == "markLinear" ||
      s == "set!" || s == "uset" || s == "setIfInBounds" || s == "set" || s == "fset" ||
      s == "swap" || s == "uswap" || s == "swapIfInBounds"
    | _ => false) ||
  fn == ``List.toArray || fn == `List.toArrayImpl || fn == `List.toArrayImpl._redArg ||
  fn == `ByteArray.mk || fn == `ByteArray.data || fn == `ByteArray.set! || fn == `ByteArray.set || fn == `ByteArray.uset ||
  fn == `FloatArray.mk || fn == `FloatArray.data || fn == `FloatArray.set! || fn == `FloatArray.set || fn == `FloatArray.uset ||
  fn == `Array.append._redArg || fn == `Array.appendCore._redArg || fn == `Array.extract._redArg ||
  fn == `Array.propagateMark._redArg || fn == `Array.markLinear._redArg

def isBuiltinFap (fn : Name) (arity : Nat) : CoreM Bool := do
  let env ← getEnv
  if isBuiltinArrayFn fn then return true
  if fn.isStr && (fn.getString!.startsWith "instInhabited" || fn.getString!.contains "inhabited") then return true
  if (getExternNameFor env `kotlin fn).isSome then return true
  if (Compiler.getKotlinMemberInfo? env fn).isSome then return true
  let prim? ← (emitPrimitiveOp? fn (Array.replicate arity .erased)).run { modName := default, localDecls := #[] } |>.run' {} |>.run (phase := .impure)
  return prim?.isSome

def isBuiltinPap (fn : Name) : CoreM Bool := do
  let targetFn := match fn with | .str p "_boxed" => p | _ => fn
  for arity in [1, 2, 3, 4, 5] do
    if ← isBuiltinFap targetFn arity then return true
  return false

partial def collectExternalCalls (code : Code .impure) (acc : Array Name := #[]) : CoreM (Array Name) := do
  match code with
  | .let decl k =>
    let mut acc := acc
    match decl.value with
    | .fap fn args =>
      unless ← isBuiltinFap fn args.size do
        acc := acc.push fn
    | .pap fn _ =>
      unless ← isBuiltinPap fn do
        let unboxed := match fn with | .str p "_boxed" => p | _ => fn
        acc := (acc.push fn).push unboxed
    | _ => pure ()
    collectExternalCalls k acc
  | .jp decl k =>
    let acc ← collectExternalCalls decl.value acc
    collectExternalCalls k acc
  | .cases c =>
    c.alts.foldlM (fun acc alt => collectExternalCalls alt.getCode acc) acc
  | c =>
    match skipCont? c with
    | some k => collectExternalCalls k acc
    | none => return acc

def runPass (inPhase outPhase : Purity) (pass : Pass) (decls : Array (Decl inPhase)) :
    CompilerM (Array (Decl outPhase)) := do
  withPhase pass.phase do
    let decls ← inPhase.withAssertPurity pass.phase.toPurity fun h =>
      pass.run (h ▸ decls)
    return pass.phaseOut.toPurity.withAssertPurity outPhase fun h => h ▸ decls

def lowerMonoDeclToImpure (monoDecl : Decl .pure) : CoreM (Array (Decl .impure)) := do
  let origSig? ← getImpureSignature? monoDecl.name
  let decls ← CompilerM.run (phase := .mono) do
    let decl ← monoDecl.internalize
    let mut decls ← runPass .pure .impure toImpure #[decl]
    decls ← runPass .impure .impure (pushProj (occurrence := 0)) decls
    decls ← runPass .impure .impure (elimDeadVars (phase := .impure) (occurrence := 0)) decls
    decls ← runPass .impure .impure simpCase decls
    decls ← runPass .impure .impure inferBorrow decls
    if let some origSig := origSig? then
      decls := decls.map fun d =>
        if d.name == monoDecl.name && d.params.size == origSig.params.size then
          let params := d.params.mapIdx fun i p => { p with borrow := origSig.params[i]!.borrow }
          { d with params }
        else d
      for d in decls do
        if d.name == monoDecl.name then
          d.saveImpure
    decls ← runPass .impure .impure explicitBoxing decls
    decls ← runPass .impure .impure explicitRc decls
    decls ← runPass .impure .impure coalesceRC decls
    decls ← runPass .impure .impure (pushProj (occurrence := 1)) decls
    return decls
  decls.mapM normalizeFVarIds

public def emitKotlinForDecls (modName : Name) (decls : Array Name) : CoreM String := do
  let (localDecls, otherModuleDecls) ← collectUsedDecls decls
  let env0 ← getEnv
  let opts ← getOptions
  let indexMap := getImpureDeclIndices env0 decls
  let localDecls := localDecls.qsort fun l r => indexMap[l.name]! < indexMap[r.name]!
  let mut declMap := localDecls.foldl (init := ({} : Std.HashMap Name (Decl .impure))) fun m d => m.insert d.name d
  let mut extDecls : Array (Decl .impure) := #[]
  let mut seenExt : Std.HashSet Name := {}
  let mut extWork : Array Name := #[]
  for d in localDecls do
    if let .code c := d.value then
      extWork ← collectExternalCalls c extWork
  while !extWork.isEmpty do
    let fn := extWork.back!
    extWork := extWork.pop
    unless declMap.contains fn || seenExt.contains fn do
      seenExt := seenExt.insert fn
      let monoDecl? ← match ← getMonoDecl? fn with
        | some md => pure (some md)
        | none => match fn with | .str p "_boxed" => getMonoDecl? p | _ => pure none
      if let some monoDecl := monoDecl? then
        if monoDecl.value matches .code _ then
          seenExt := seenExt.insert monoDecl.name
          let lowered ← lowerMonoDeclToImpure monoDecl
          for d in lowered do
            unless declMap.contains d.name do
              declMap := declMap.insert d.name d
              extDecls := extDecls.push d
              if let .code c := d.value then
                extWork ← collectExternalCalls c extWork
  let env ← getEnv
  let allDecls := localDecls ++ extDecls
  let fileSpec? ← match ← findFileSpecDecl? env with
    | some n => some <$> evalFileSpec n
    | none => pure none
  let readOpt (inline file : String) : CoreM String := do
    let fileContent ← if file.isEmpty then pure "" else IO.FS.readFile file
    return String.intercalate "\n\n" ([inline, fileContent].filter (!·.isEmpty))
  let headerText ← readOpt (compiler.kotlin.preamble.get opts) (compiler.kotlin.preamble_file.get opts)
  let footerText ← readOpt (compiler.kotlin.footer.get opts) (compiler.kotlin.footer_file.get opts)
  let specText := match fileSpec? with
    | some spec => String.intercalate "\n" (fileSpecTexts spec).toList
    | none => ""
  let prune := compiler.kotlin.pruneUnreachable.get opts
  let roots :=
    if !prune then
      localDecls
    else
      let tokens := identTokens (headerText ++ "\n" ++ footerText ++ "\n" ++ specText)
      localDecls.filter fun d =>
        (Compiler.getKotlinMemberInfo? env d.name).isSome || isExport env d.name ||
          tokens.contains (toKotlinFnName d.name)
  let mut visited : Std.HashSet Name := {}
  -- Declarations that are fully applied somewhere (as opposed to only partially applied).
  let mut applied : Std.HashSet Name := {}
  let mut work := roots.map (·.name)
  while !work.isEmpty do
    let n := work.back!
    work := work.pop
    unless visited.contains n do
      visited := visited.insert n
      if let some d := declMap[n]? then
        if let .code c := d.value then
          for m in collectCalls env declMap c #[] do
            if declMap.contains m && !visited.contains m then
              work := work.push m
          for m in collectCalls env declMap (paps := false) c #[] do
            applied := applied.insert m
  let rootNames := roots.map (·.name)
  -- Loop-expandable declarations are emitted inline at every call site, and declarations
  -- that are only partially applied are beta-inlined at every application.
  let emittedLocal :=
    if !prune then localDecls
    else localDecls.filter fun d =>
      visited.contains d.name && !isLoopExpandable env d && (isInlinedConstDecl? d).isNone &&
        (applied.contains d.name || rootNames.contains d.name)
  let emittedExt := extDecls.filter fun d =>
    visited.contains d.name && !isLoopExpandable env d && (isInlinedConstDecl? d).isNone &&
      applied.contains d.name
  let toEmit := emittedLocal ++ emittedExt
  let classStructs := env.constants.map₂.foldl (init := ({} : Std.HashMap String Name)) fun m n _ =>
    match Compiler.getKotlinClassSpec? env n with
    | some spec =>
      let m := m.insert spec.className n |>.insert spec.baseClassName n
      match isInductiveCore? env n with
      | some iv =>
        if iv.numParams > 0 then
          let stars := String.intercalate ", " (List.replicate iv.numParams "*")
          m.insert s!"{spec.baseClassName}<{stars}>" n
        else m
      | none => m
    | none =>
      match Compiler.getMutableKotlinClass? env n with
      | some t => m.insert t n |>.insert (({ recvType := t : Compiler.KotlinMemberInfo }).className) n
      | none => m
  -- In-place updates must be justified by exclusive ownership.
  let papTargets := allDecls.foldl (init := ({} : Std.HashSet Name)) fun acc d =>
    match d.value with
    | .code c => collectPaps c acc
    | _ => acc
  let isMember (n : Name) := (Compiler.getKotlinMemberInfo? env n).isSome
  let ownership ← (Ownership.analyze allDecls isMember
    (fun n => isMember n || isExport env n || papTargets.contains n)).run (phase := .impure)
  unless ownership.errors.isEmpty do
    throwError (MessageData.joinSep ownership.errors.toList "\n")
  let extDeclNames := extDecls.foldl (init := ({} : Std.HashSet Name)) fun s d => s.insert d.name
  let ctx : Context := { modName, localDecls := allDecls, otherModuleDecls, declMap,
                         summaries := ownership.summaries, classStructs, extDeclNames }
  let ((), st) ← ((do
    let mut topBuf := ""
    let mut memberBufs : Std.HashMap String String := {}
    for d in toEmit do
      let s ← captureBuf (emitFnDecl d)
      match Compiler.getKotlinMemberInfo? env d.name with
      | some info => memberBufs := memberBufs.insert info.className (memberBufs.getD info.className "" ++ s)
      | none => topBuf := topBuf ++ s
    let pkg := compiler.kotlin.package.get opts
    let pkgName :=
      if !pkg.isEmpty then pkg
      else
        let parts := modName.components
        if parts.length > 1 then
          String.intercalate "." (parts.dropLast.map (·.toString))
        else
          ""
    let hasUnsignedArrays :=
      topBuf.contains "UByteArray" || topBuf.contains "UShortArray" || topBuf.contains "UIntArray" || topBuf.contains "ULongArray" ||
      memberBufs.values.any (fun s => s.contains "UByteArray" || s.contains "UShortArray" || s.contains "UIntArray" || s.contains "ULongArray")
    let optInUnsigned := if hasUnsignedArrays then "\n@file:OptIn(ExperimentalUnsignedTypes::class)" else ""
    let suppress := "@file:Suppress(\"UNCHECKED_CAST\", \"UNUSED_VARIABLE\", \"NAME_SHADOWING\", \"RemoveRedundantBackticks\", \"ConstantConditionIf\", \"RedundantExplicitType\", \"RedundantCallOfConversionMethod\", \"USELESS_CAST\", \"NOTHING_TO_INLINE\", \"UNREACHABLE_CODE\", \"UNUSED_PARAMETER\", \"UNUSED_EXPRESSION\", \"SENSELESS_COMPARISON\")" ++ optInUnsigned
    match fileSpec? with
    | some spec =>
      let classNames := spec.items.filterMap fun | .cls c => some c.name | _ => none
      for cls in memberBufs.keys do
        unless classNames.contains cls do
          throwError "Kotlin backend: `@[kotlin_member]` of class `{cls}`, which is not declared in the `@[kotlin_file]` spec"
      if !spec.preamble.isEmpty then
        emit (reindent "" spec.preamble)
        emitLn ""
      emitLn "// Generated automatically by the Lean 4 Kotlin backend. DO NOT EDIT DIRECTLY."
      emitLn suppress
      for a in spec.fileAnnotations do emitLn a
      emitLn ""
      if !pkgName.isEmpty then
        emitLn s!"package {pkgName}"
        emitLn ""
      if !spec.imports.isEmpty then
        for i in spec.imports do emitLn i
        emitLn ""
      let mut first := true
      for item in spec.items do
        let text : String := match item with
          | .verbatim code => reindent "" code
          | .topLevel => topBuf
          | .cls c => Id.run do
            let mut body := ""
            let mut prevField := false
            let mut firstItem := true
            for i in c.body do
              let isField := match i with | .field _ => true | _ => false
              let piece := match i with
                | .field d => reindent "    " d
                | .init b => "    init {\n" ++ reindent "        " b ++ "    }\n"
                | .members => reindent "    " (memberBufs.getD c.name "")
                | .verbatim code => reindent "    " code
              if piece.isEmpty then continue
              unless firstItem || (isField && prevField) do body := body ++ "\n"
              body := body ++ piece
              firstItem := false
              prevField := isField
            let hdr := String.intercalate "\n" (trimBlankLines c.header)
            return hdr ++ " {\n" ++ body ++ "}\n"
        if text.isEmpty then continue
        unless first do emitLn ""
        emit text
        first := false
    | none =>
      let (hdr, usedH) := spliceMembers headerText memberBufs
      let (ftr, usedF) := spliceMembers footerText memberBufs
      let hasClassDecl (text cls : String) : Bool :=
        text.contains s!"class {cls}(" || text.contains s!"class {cls} " || text.contains s!"class {cls}\{" ||
        text.contains s!"interface {cls} " || text.contains s!"interface {cls}\{"
      let localClassStructs :=
        (Compiler.kotlinClassAttr.ext.getState env).1.reverse ++
        (Compiler.mutableKotlinClassAttr.ext.getState env).1.reverse
      let mut synthClasses : Std.HashSet String := {}
      let mut synthBuf := ""
      for s in localClassStructs do
        if (← hasTrivialImpureStructure? s).isSome then continue
        let some rawCls := Compiler.getKotlinClass? env s | continue
        let cls := ({ recvType := rawCls : Compiler.KotlinMemberInfo }).className
        if usedH.contains cls || usedF.contains cls || synthClasses.contains cls ||
           hasClassDecl headerText cls || hasClassDecl footerText cls then
          continue
        synthClasses := synthClasses.insert cls
        let spec := (Compiler.getKotlinClassSpec? env s).getD { raw := rawCls, className := rawCls }
        let members := memberBufs.getD cls ""
        let clsBuf ← captureBuf do
          if let some (.inl doc) ← findInternalDocString? env s (includeBuiltin := false) then
            for l in kdocLines doc do emitLn l
          if Compiler.isKotlinInductive env s then
            let some iv := isInductiveCore? env s | return
            if Compiler.isKotlinEnum env s then
              let entries := iv.ctors.map fun ctorName => toPascalCase ctorName.getString!
              let entriesStr := String.intercalate ",\n    " entries
              if members.isEmpty then
                emitLn s!"enum class {cls} \{"
                emitLn s!"    {entriesStr};"
                emitLn "}"
                emitLn ""
              else
                emitLn s!"enum class {cls} \{"
                emitLn s!"    {entriesStr};"
                emitLn ""
                emit (reindent "    " members)
                emitLn "}"
                emitLn ""
            else
              let isSealedClass := spec.kind? == some "sealed class"
              let typeParams ← (Meta.MetaM.run' (getDeclTypeParams env s) : CoreM (Array (Name × String)))
              let typeParamNames := typeParams.map (·.2)
              let typeParamStr :=
                if !typeParamNames.isEmpty && !cls.contains '<' then
                  s!"<out {String.intercalate ", out " typeParamNames.toList}>"
                else ""
              let outerHeader := if isSealedClass then s!"sealed class {cls}{typeParamStr}" else s!"sealed interface {cls}{typeParamStr}"
              let dataMod := if spec.isData then "data " else ""
              emitLn s!"{outerHeader} \{"
              for ctorName in iv.ctors do
                let some (.ctorInfo cv) := env.find? ctorName | continue
                let variantName := toPascalCase ctorName.getString!
                if cv.numFields == 0 then
                  let baseExtends :=
                    if !typeParamNames.isEmpty then
                      let nothingArgs := s!"<{String.intercalate ", " (typeParamNames.map fun _ => "Nothing").toList}>"
                      if isSealedClass then s!" : {cls}{nothingArgs}()" else s!" : {cls}{nothingArgs}"
                    else
                      if isSealedClass then s!" : {cls}()" else s!" : {cls}"
                  emitLn s!"    {dataMod}object {variantName}{baseExtends}"
                else
                  let ctorLayout ← ctorClassLayout ctorName
                  let propStr := String.intercalate ", " (ctorLayout.orderedFields.map fun (f, t) =>
                    let propName := if isKotlinKeyword f then s!"`{f}`" else f
                    s!"val {propName}: {t}").toList
                  let subTypeParamStr :=
                    if !typeParamNames.isEmpty then
                      s!"<out {String.intercalate ", out " typeParamNames.toList}>"
                    else ""
                  let subTypeArgs :=
                    if !typeParamNames.isEmpty then
                      s!"<{String.intercalate ", " typeParamNames.toList}>"
                    else ""
                  let baseExtends :=
                    if isSealedClass then s!" : {cls}{subTypeArgs}()" else s!" : {cls}{subTypeArgs}"
                  emitLn s!"    {dataMod}class {variantName}{subTypeParamStr}({propStr}){baseExtends}"
              if !members.isEmpty then
                emitLn ""
                emit (reindent "    " members)
              emitLn "}"
              emitLn ""
          else
            let isMut := Compiler.isMutableKotlinClass env s
            let propKw := if isMut then "var" else "val"
            let layout ← classLayout s
            let propStr := String.intercalate ", " (layout.orderedFields.map fun (f, t) =>
              let propName := if isKotlinKeyword f then s!"`{f}`" else f
              s!"{propKw} {propName}: {t}").toList
            let typeParamStr :=
              if !layout.typeParams.isEmpty && !cls.contains '<' then
                s!"<{String.intercalate ", " layout.typeParams.toList}>"
              else ""
            let dataPrefix := if spec.isData then "data " else ""
            if members.isEmpty then
              emitLn s!"{dataPrefix}class {cls}{typeParamStr}({propStr})"
              emitLn ""
            else
              emitLn s!"{dataPrefix}class {cls}{typeParamStr}({propStr}) \{"
              emit (reindent "    " members)
              emitLn "}"
              emitLn ""
        synthBuf := synthBuf ++ clsBuf
      for cls in memberBufs.keys do
        unless usedH.contains cls || usedF.contains cls || synthClasses.contains cls do
          throwError "Kotlin backend: no `// @LeanMembers({cls})` marker in the preamble or footer"
      emitLn "// Generated automatically by Lean 4 to Kotlin compiler backend."
      emitLn "// DO NOT EDIT DIRECTLY."
      emitLn suppress
      if !pkgName.isEmpty then
        emitLn s!"package {pkgName}"
        emitLn ""
      if !headerText.isEmpty then
        emit hdr
        emitLn ""
      emit synthBuf
      emit topBuf
      if !footerText.isEmpty then
        emitLn ""
        emit ftr
  ) : EmitM Unit).run ctx |>.run {} |>.run (phase := .impure)
  return st.buf

public def emitKotlin (modName : Name) : CoreM String := do
  emitKotlinForDecls modName (← getLocalImpureDecls)

end Kotlin

public def emitKotlinForDecls (modName : Name) (decls : Array Name) : CoreM String :=
  Kotlin.emitKotlinForDecls modName decls

public def emitKotlin (modName : Name) : CoreM String :=
  Kotlin.emitKotlin modName

end Lean.Compiler.LCNF
