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
public import Lean.Compiler.KotlinSpec
import Lean.DocString.Extension
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
  /-- Precomputed variable aliases for the current function or loop body. -/
  aliases : Std.HashMap FVarId FVarId := {}
  /-- Recorded exit expressions for each loop label. -/
  loopExits : Std.HashMap String (Array String) := {}

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

def toKotlinType (ty : Expr) : String :=
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
  | ImpureType.void => "Unit"
  | .const ``Bool _ | .const ``Decidable _ => "Boolean"
  | .const ``Unit _ | .const ``PUnit _ => "Unit"
  | .app (.const `jvmType _) (.lit (.strVal desc)) =>
    -- Types declared with `@[extern "kotlin:<Kotlin type>"]`.
    if desc.startsWith "kotlin:" then (desc.drop 7).toString else "Any?"
  | _ => "Any?"

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
  | "Unit" => "Unit"
  | _ => if ty.endsWith "?" then "null" else s!"(null as {ty})"

def recordVarType (name : String) (ty : String) : EmitM Unit := do
  if ty != "Any?" && ty != "_" then
    modify fun st => { st with varTypes := st.varTypes.insert name ty }

def isKotlinIntType (ty : String) : Bool :=
  ty == "Byte" || ty == "Short" || ty == "Int" || ty == "Long" ||
  ty == "UByte" || ty == "UShort" || ty == "UInt" || ty == "ULong"

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
  if actualTy? == some targetTy || (actualTy?.map (s!"{·}?") == some targetTy) then
    return s
  if let some actualTy := actualTy? then
    if isKotlinIntType actualTy then
      if let some conv := kotlinIntConversionMethod targetTy then
        return s!"({s}).{conv}"
  return s!"({s} as {targetTy})"

def getVarName (fvarId : FVarId) : EmitM String := do
  if let some name := (← get).varNames[fvarId]? then
    return name
  let rawName := (← getBinderName fvarId).toString
  let cleanName := rawName.replace "." "_"
  let cleanName := if cleanName.startsWith "_" then "v" ++ cleanName else cleanName
  let count := (← get).nameCounter + 1
  let uniqueName := s!"{cleanName}_{count}"
  modify fun st => {
    st with
    varNames := st.varNames.insert fvarId uniqueName
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

def emitPrimitiveOp? (fn : Name) (args : Array (Arg .impure)) : EmitM (Option String) := do
  if args.size == 1 then
    let a0 ← toKotlinArg args[0]!
    let a0Ty? := (← get).varTypes[a0]?
    match fn with
    -- Conversions to Int8 (Byte)
    | ``Int8.ofNat | ``Int8.ofInt | ``UInt8.toInt8 | ``Int16.toInt8 | ``Int32.toInt8 | ``Int64.toInt8 | ``ISize.toInt8 =>
      return some (if a0Ty? == some "Byte" then a0 else s!"({a0}).toByte()")
    | ``Bool.toInt8 =>
      return some s!"(if ({a0}) (1).toByte() else (0).toByte())"
    -- Conversions to Int16 (Short)
    | ``Int16.ofNat | ``Int16.ofInt | ``UInt16.toInt16 | ``Int8.toInt16 | ``Int32.toInt16 | ``Int64.toInt16 | ``ISize.toInt16 =>
      return some (if a0Ty? == some "Short" then a0 else s!"({a0}).toShort()")
    | ``Bool.toInt16 =>
      return some s!"(if ({a0}) (1).toShort() else (0).toShort())"
    -- Conversions to Int32 / ISize / USize / Nat (Int)
    | ``Int.toNat | ``Int8.toInt | ``Int16.toInt | ``Int32.toInt | ``Int64.toInt
    | ``Int.toInt32 | ``Int.toInt64
    | ``Int32.ofNat | ``Int32.ofInt | ``ISize.ofNat | ``ISize.ofInt | ``USize.ofNat
    | ``UInt32.toInt32 | ``Int8.toInt32 | ``Int16.toInt32 | ``Int64.toInt32 | ``ISize.toInt32
    | ``Int32.toISize | ``Int64.toISize | ``UInt32.toUSize | ``UInt64.toUSize
    | ``UInt8.toNat | ``UInt16.toNat | ``UInt32.toNat | ``UInt64.toNat | ``USize.toNat
    | ``Int8.toNatClampNeg | ``Int16.toNatClampNeg | ``Int32.toNatClampNeg | ``Int64.toNatClampNeg =>
      return some (if a0Ty? == some "Int" then a0 else s!"({a0}).toInt()")
    | ``Bool.toInt32 | ``Bool.toISize | ``Bool.toNat | ``Bool.toUSize =>
      return some s!"(if ({a0}) 1 else 0)"
    -- Conversions to Int64 (Long)
    | ``Int64.ofNat | ``Int64.ofInt | ``UInt64.toInt64 | ``Int8.toInt64 | ``Int16.toInt64 | ``Int32.toInt64 | ``ISize.toInt64 =>
      return some (if a0Ty? == some "Long" then a0 else s!"({a0}).toLong()")
    | ``Bool.toInt64 =>
      return some s!"(if ({a0}) 1L else 0L)"
    -- Conversions to UInt8 (UByte)
    | ``UInt8.ofNat | ``Int8.toUInt8 | ``UInt16.toUInt8 | ``UInt32.toUInt8 | ``UInt64.toUInt8 | ``USize.toUInt8 =>
      return some (if a0Ty? == some "UByte" then a0 else s!"({a0}).toUByte()")
    | ``Bool.toUInt8 =>
      return some s!"(if ({a0}) (1u).toUByte() else (0u).toUByte())"
    -- Conversions to UInt16 (UShort)
    | ``UInt16.ofNat | ``Int16.toUInt16 | ``UInt8.toUInt16 | ``UInt32.toUInt16 | ``UInt64.toUInt16 | ``USize.toUInt16 =>
      return some (if a0Ty? == some "UShort" then a0 else s!"({a0}).toUShort()")
    | ``Bool.toUInt16 =>
      return some s!"(if ({a0}) (1u).toUShort() else (0u).toUShort())"
    -- Conversions to UInt32 (UInt)
    | ``UInt32.ofNat | ``Int32.toUInt32 | ``UInt8.toUInt32 | ``UInt16.toUInt32 | ``UInt64.toUInt32 | ``USize.toUInt32 =>
      return some (if a0Ty? == some "UInt" then a0 else s!"({a0}).toUInt()")
    | ``Bool.toUInt32 =>
      return some s!"(if ({a0}) 1u else 0u)"
    -- Conversions to UInt64 (ULong)
    | ``UInt64.ofNat | ``Int64.toUInt64 | ``UInt8.toUInt64 | ``UInt16.toUInt64 | ``UInt32.toUInt64 | ``USize.toUInt64 =>
      return some (if a0Ty? == some "ULong" then a0 else s!"({a0}).toULong()")
    | ``Bool.toUInt64 =>
      return some s!"(if ({a0}) 1uL else 0uL)"
    -- Unary negation
    | ``Int32.neg | ``Int64.neg | ``ISize.neg =>
      return some s!"(-{a0})"
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
    -- Bitwise complement
    | ``Int8.complement | ``Int16.complement | ``Int32.complement | ``Int64.complement | ``ISize.complement
    | ``UInt8.complement | ``UInt16.complement | ``UInt32.complement | ``UInt64.complement | ``USize.complement =>
      return some s!"{a0}.inv()"
    | _ =>
      if fn.isStr && (fn.getString! == "ofNat" || fn.getString! == "toUInt64") then
        if a0Ty? == some "Long" then return some a0
        if a0Ty? == some "Int" then return some s!"{a0}.toLong()"
        return some s!"({a0} as Number).toLong()"
      if fn.isStr && fn.getString! == "toUInt32" then
        if a0Ty? == some "Int" then return some a0
        if a0Ty? == some "Long" then return some s!"{a0}.toInt()"
        return some s!"({a0} as Number).toInt()"
      return none
  if args.size == 2 then
    let a0 ← toKotlinArg args[0]!
    let a1 ← toKotlinArg args[1]!
    let a1Int :=
      if (← get).varTypes[a1]? == some "Int" then a1
      else if a1.endsWith "uL" && a1.length > 2 && (a1.take (a1.length - 2)).all Char.isDigit then (a1.take (a1.length - 2)).toString
      else if (a1.endsWith "L" || a1.endsWith "u") && a1.length > 1 && (a1.take (a1.length - 1)).all Char.isDigit then (a1.take (a1.length - 1)).toString
      else s!"({a1}).toInt()"
    match fn with
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

    | ``Bool.decEq => return some s!"({a0} == {a1})"
    | ``Nat.decEq =>
      return some s!"({← castIfNeeded a0 "Int"} == {← castIfNeeded a1 "Int"})"
    | ``Nat.decLt =>
      return some s!"({← castIfNeeded a0 "Int"} < {← castIfNeeded a1 "Int"})"
    | ``Nat.decLe =>
      return some s!"({← castIfNeeded a0 "Int"} <= {← castIfNeeded a1 "Int"})"
    | `Nat.shiftLeft =>
      return some s!"({← castIfNeeded a0 "Int"} shl {a1Int})"
    | `Nat.add =>
      return some s!"({← castIfNeeded a0 "Int"} + {← castIfNeeded a1 "Int"})"
    | `Nat.sub =>
      return some s!"(maxOf(0, {← castIfNeeded a0 "Int"} - {← castIfNeeded a1 "Int"}))"
    | `Nat.mul =>
      return some s!"({← castIfNeeded a0 "Int"} * {← castIfNeeded a1 "Int"})"
    | `Nat.div =>
      return some s!"({← castIfNeeded a0 "Int"} / {← castIfNeeded a1 "Int"})"
    | `Nat.mod =>
      return some s!"({← castIfNeeded a0 "Int"} % {← castIfNeeded a1 "Int"})"
    | _ => return none
  return none

/--
Reference to field `name` of `recv`. On `this`, the field is referenced unqualified unless a
parameter of the current function has the same name.
-/
def fieldRef (recv name : String) : EmitM String := do
  if recv != "this" then return s!"{recv}.{name}"
  let shadowed ← (← read).currParams.anyM fun p => return (← getVarName p.fvarId) == name
  return if shadowed then s!"this.{name}" else name

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
  | some d => return if d.startsWith "kotlin:" then some (d.drop 7).toString else none
  | none => return none

def arrayType (a : Arg .impure) : EmitM String := do
  let some t ← kotlinTypeOf? a
    | throwError "Kotlin backend: in `{(← read).currFn}`: `Array` operations require `set_option compiler.kotlin.typedArrays true`"
  return t

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

/-- Reads of `Array`s (with `compiler.kotlin.typedArrays`). -/
def emitArrayRead? (fn : Name) (args : Array (Arg .impure)) (resTy : Expr) : EmitM (Option String) := do
  let arg (i : Nat) : EmitM String := toKotlinArg (args[i]?.getD .erased)
  if fn == arrOp "get!Internal" || fn == arrOp "get!InternalBorrowed" then
    discard <| arrayType (args[2]?.getD .erased)
    return some s!"{← arg 2}[{← castIfNeeded (← arg 3) "Int"}]"
  if fn == arrOp "getInternal" || fn == arrOp "getInternalBorrowed" then
    discard <| arrayType (args[1]?.getD .erased)
    return some s!"{← arg 1}[{← castIfNeeded (← arg 2) "Int"}]"
  if fn == arrOp "uget" || fn == arrOp "ugetBorrowed" then
    discard <| arrayType (args[1]?.getD .erased)
    return some s!"{← arg 1}[{← castIfNeeded (← arg 2) "Int"}]"
  if fn == arrOp "size" || fn == arrOp "usize" then
    discard <| arrayType (args[1]?.getD .erased)
    return some s!"{← arg 1}.size"
  if fn == arrOp "replicate" || fn == arrOp "mkArray" then
    let some d := getJvmTypeDesc? resTy
      | throwError "Kotlin backend: in `{(← read).currFn}`: `Array` operations require `set_option compiler.kotlin.typedArrays true` (result type `{resTy}`)"
    return some (← kotlinArrayAlloc (d.drop 7).toString (← castIfNeeded (← arg 1) "Int") (← arg 2))
  if fn == arrOp "mkEmpty" || fn == arrOp "emptyWithCapacity" then
    let some d := getJvmTypeDesc? resTy
      | throwError "Kotlin backend: in `{(← read).currFn}`: `Array` operations require `set_option compiler.kotlin.typedArrays true`"
    let t := (d.drop 7).toString
    return some (if (kotlinArrayElem? t).isSome then s!"{t}(0)" else "arrayOfNulls<Any?>(0)")
  if fn.getPrefix == `Array && (fn matches .str _ _) then
    let supported := ["set!", "uset", "setIfInBounds"]
    unless supported.contains fn.getString! do
      throwError "Kotlin backend: `{fn}` is not supported (Kotlin arrays have a fixed size)"
  return none

/-- In-place `Array` updates: `(array arg, index expr, value arg)`. -/
def arraySet? (fn : Name) (args : Array (Arg .impure)) : EmitM (Option (Arg .impure × String × Arg .impure)) := do
  if fn == arrOp "set!" || fn == arrOp "setIfInBounds" then
    return some (args[1]!, ← castIfNeeded (← toKotlinArg args[2]!) "Int", args[3]!)
  if fn == arrOp "uset" then
    return some (args[1]!, ← castIfNeeded (← toKotlinArg args[2]!) "Int", args[3]!)
  return none

/-- Kotlin property names of a `@[kotlin_class]` structure by runtime position. -/
structure ClassLayout where
  objs : Std.HashMap Nat String := {}
  /-- By byte offset. -/
  scalars : Std.HashMap Nat String := {}
  usizes : Std.HashMap Nat String := {}

def classLayout (s : Name) : EmitM ClassLayout := do
  let env ← getEnv
  let fields := getStructureFields env s
  let layout ← getCtorLayout (getStructureCtor env s).name
  let mut r : ClassLayout := {}
  for h : k in [:layout.fieldInfo.size] do
    let name := match fields[k]? with
      | some (.str _ n) => n
      | _ => s!"field{k}"
    match layout.fieldInfo[k] with
    | .object i _ => r := { r with objs := r.objs.insert i name }
    | .scalar _ off _ => r := { r with scalars := r.scalars.insert off name }
    | .usize i => r := { r with usizes := r.usizes.insert i name }
    | _ => pure ()
  return r

/-- The `@[kotlin_class]` structure of variable `x`, from its Kotlin type. -/
def classStructOf? (x : FVarId) : EmitM (Option Name) := do
  let n ← getVarName x
  if let some t ← kotlinTypeOf? (.fvar x) then
    if let some s := (← read).classStructs[t]? then
      modify fun st => { st with nameStructs := st.nameStructs.insert n s }
      return some s
  -- Variables aliased to a `@[kotlin_class]` value (e.g. by `reset`) share its Kotlin name.
  return (← get).nameStructs[n]?

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
  if let some d := (← read).declMap[fn]? then
    if d.type.isScalar then return none
  let some s := (← read).summaries[fn]? | return none
  if s.ret.identities.isEmpty then return none
  return some s.ret

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
    let impure ← (Meta.MetaM.run' do
      let t ← toLCNFType comp
      let t ← toMonoType t
      toImpureType t : CoreM Expr)
    let t := toKotlinType impure
    return if t == "Any?" then none else some t
  catch _ => return none

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
  if let some ci := env.find? fn then
    if resultType ci.type == mkConst ``Unit then return "Unit"
  if let some d := (← read).declMap[fn]? then
    let t := toKotlinType d.type
    if t != "Any?" then return t
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
  if extStr.startsWith "kotlin_expr:" then
    let argStrs ← args.mapM toKotlinArg
    return some (instantiateKotlinTemplate (extStr.drop 12).toString argStrs)
  if extStr.startsWith "kotlin_inplace:" then
    let argStrs ← args.mapM toKotlinArg
    return some (instantiateKotlinTemplate (extStr.drop 15).toString argStrs)
  unless extStr.startsWith "kotlin_op:" do return none
  let op := (extStr.drop 10).toString
  let argStrs ← args.mapM toKotlinArg
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
  let recv ← toKotlinArg args[0]!
  let tyOverrides := (Compiler.getKotlinTypes? (← getEnv) fn).getD #[]
  let rest ← (args.extract 1 args.size).mapIdxM fun i a => do
    let s ← toKotlinArg a
    match tyOverrides[i]? with
    | some t => if t == "_" then pure s else castIfNeeded s t
    | none => pure s
  let call := s!"{memberKotlinName fn info}({String.intercalate ", " rest.toList})"
  if recv == "this" then
    if (← read).currClass? == some info.className then
      return some call
    return some s!"{← castIfNeeded "this" info.recvType}.{call}"
  let sameType ← match args[0]! with
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
    | .nat n =>
      if n > 2147483647 then
        let signedVal : Int := (n : Int) - 4294967296
        return s!"({signedVal})"
      else
        return s!"{n}"
    | .str s => return s!"\"{s}\""
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
    let argStrs ← args.mapM toKotlinArg
    let argStr := String.intercalate ", " argStrs.toList
    return s!"{fnName}({argStr})"
  | .fvar fvarId args =>
    let fnName ← getVarName fvarId
    let argStrs ← args.mapM toKotlinArg
    let argStr := String.intercalate ", " argStrs.toList
    return s!"{fnName}({argStr})"
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
    if (← classStructOf? x).isSome then classField x (·.objs[i]?) else unsupportedProj x
  | .sproj _ off x =>
    if (← classStructOf? x).isSome then classField x (·.scalars[off]?) else unsupportedProj x
  | .uproj i x =>
    if (← classStructOf? x).isSome then classField x (·.usizes[i]?) else unsupportedProj x
  | .ctor info _ =>
    if let some s := (← read).classStructs.values.find? (· == info.name.getPrefix) then
      throwError "Kotlin backend: in `{(← read).currFn}`: cannot allocate a new `{s}` (`@[kotlin_class]` values are only updated in place)"
    return "null"
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

/-- Declarations called (or, if `paps`, also partially applied) in `code`. -/
partial def collectCalls (code : Code .impure) (acc : Array Name) (paps := true) : Array Name :=
  match code with
  | .let decl k =>
    let acc := match decl.value with
      | .fap n _ => acc.push n
      | .pap n _ => if paps then acc.push n else acc
      | _ => acc
    collectCalls k acc paps
  | .jp decl k => collectCalls k (collectCalls decl.value acc paps) paps
  | .cases c => c.alts.foldl (fun acc alt => collectCalls alt.getCode acc paps) acc
  | c => match skipCont? c with | some k => collectCalls k acc paps | none => acc

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
  return some (d, papArgs ++ args)

/-- Records `let x := pap fn args` of a local code declaration, emitting nothing. -/
def recordPap? (decl : LetDecl .impure) : EmitM Bool := do
  let .pap fn args := decl.value | return false
  let some d := (← read).declMap[fn]? | return false
  let .code _ := d.value | return false
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

/-- Returns from the current `return` target without a value. -/
def emitReturnUnit : EmitM Unit := do
  match ← currentExit with
  | .funcReturn => emitLn "return"
  | .inlineAlias fv => setParamVarName fv "Unit"
  | .assignOnly x _ => emitLn s!"{x} = Unit"
  | .assignAndBreak _ _ loopLbl => emitLn s!"break@{loopLbl}"
  | .breakOnly loopLbl => emitLn s!"break@{loopLbl}"
  | .returnLbl lbl _ => emitLn s!"return@{lbl} Unit"

/-- `return x`, dropping the components of the result that are identical to parameters. -/
def emitReturnVar (x : FVarId) : EmitM Unit := do
  let rawName ← getVarName x
  let n := match (← get).knownBools[rawName]? with
    | some true => "true"
    | some false => "false"
    | none => rawName
  match (← read).retShape? with
  | some (.param _) => emitReturnUnit
  | some shape@(.prod _) =>
    let some comps := (← get).tuples[n]?
      | throwError "Kotlin backend: in `{(← read).currFn}`: expected a `Prod.mk` result"
    match keptComps shape with
    | #[] => emitReturnUnit
    | #[c] =>
      let rawComp := comps[c]!
      let comp := match (← get).knownBools[rawComp]? with
        | some true => "true"
        | some false => "false"
        | none => rawComp
      emitReturn comp (← get).varTypes[comp]?
    | _ => throwError "Kotlin backend: in `{(← read).currFn}`: results with more than one \
      component that is not a parameter are not supported"
  | _ =>
    if (← get).tuples.contains n then
      throwError "Kotlin backend: in `{(← read).currFn}`: `Prod` values are only supported as \
        results whose other components are parameters"
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
  if (← classStructOf? x).isSome then
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
    | .ctor info args =>
      if !isClassCtor info.name.getPrefix then
        collectWriteBacks env isClassCtor k (killArgs projs args)
      else
        -- Field assignments `(kind, pos, value)`: object arguments, then the trailing `sset`/`uset`s.
        let rec scalars (c : Code .impure) (r : Array (Nat × Nat × FVarId)) :=
          match c with
          | .sset y _ off v _ c _ => scalars c (if y == x then r.push (1, off, v) else r)
          | .uset y i v c _ => scalars c (if y == x then r.push (2, i, v) else r)
          | .inc (k := c) .. | .dec (k := c) .. => scalars c r
          | _ => r
        let objs := args.mapIdx fun i a => match a with
          | .fvar v => some (0, i, v)
          | _ => none
        let assigns := objs.filterMap id ++ scalars k #[]
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
  let structs := (← read).classStructs
  let isClassCtor (s : Name) := structs.values.contains s
  let wb := ((collectWriteBacks env isClassCtor code {}).run { acc := (← get).writeBacks }).2.acc
  modify fun st => { st with writeBacks := wb, used := collectUsed wb code st.used }

def isWriteBack (x v : FVarId) : EmitM Bool :=
  return (← get).writeBacks.contains (x, v)

structure AliasState where
  aliases : Std.HashMap FVarId FVarId := {}
  projs : Std.HashMap FVarId ProjInfo := {}
  /-- Components of `Prod` results that are aliases. -/
  comps : Std.HashMap (FVarId × Nat) FVarId := {}
  /-- Roots of the arguments passed to each join point parameter (`none`: not a variable). -/
  jmpArgs : Std.HashMap FVarId (Array (Option FVarId)) := {}

/--
Variables of `code` that are emitted as Kotlin aliases of another variable, mapped to the root:
in-place updates of `@[kotlin_class]` values (aliases of the unique source of their projected
fields), results of calls identical to an argument (see `registerDropped`), and join point
parameters all of whose arguments are aliases of the same variable.
-/
partial def collectAliases (code : Code .impure) : EmitM (Std.HashMap FVarId FVarId) := do
  let structs := (← read).classStructs
  let root (f : FVarId) : StateT AliasState EmitM FVarId := return (← get).aliases.getD f f
  let addAlias (x r : FVarId) : StateT AliasState EmitM Unit :=
    modify fun (s : AliasState) => { s with aliases := s.aliases.insert x r }
  let addProj (x : FVarId) (p : ProjInfo) : StateT AliasState EmitM Unit :=
    modify fun (s : AliasState) => { s with projs := s.projs.insert x p }
  let rec go (c : Code .impure) : StateT AliasState EmitM Unit := do
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
        if structs.values.contains info.name.getPrefix then
          let vs := args.filterMap (fun a => match a with | .fvar f => some f | _ => none)
            ++ Ownership.ctorScalars x k
          let roots ← vs.mapM root
          let projs := (← get).projs
          let srcs := roots.filterMap fun v => projs[v]?.map (·.2.2)
          if let some y := srcs[0]? then
            if srcs.all (· == y) then addAlias x (← root y)
      | .fap fn args =>
        if fn == arrOp "set!" || fn == arrOp "setIfInBounds" || fn == arrOp "uset" then
          if let some (.fvar y) := (args[1]? : Option (Arg .impure)) then addAlias x (← root y)
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
      go k
    | .jp decl k | .fun decl k _ => go decl.value; go k
    | .cases cs => for alt in cs.alts do go alt.getCode
    | .jmp fn args =>
      let some (d : FunDecl .impure) ← findFunDecl? fn | return
      for h : i in [:d.params.size] do
        let r ← match args[i]? with
          | some (.fvar y) => some <$> root y
          | _ => pure none
        let pid := d.params[i].fvarId
        modify fun (s : AliasState) => { s with jmpArgs := s.jmpArgs.insert pid ((s.jmpArgs.getD pid #[]).push r) }
    | c => if let some k := skipCont? c then go k
  -- Join point bodies precede their jumps: a multi-pass fixed point sees nested parameter aliases.
  let mut seed : Std.HashMap FVarId FVarId := {}
  let mut aliases : Std.HashMap FVarId FVarId := {}
  for _ in [0:4] do
    let ((), st) ← (go code).run ({ aliases := seed } : AliasState)
    aliases := st.aliases
    for (pid, rs) in st.jmpArgs do
      if let some (some r) := rs[0]? then
        if rs.all (· == some r) && r != pid then
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
  | .usize _ | .nat _ => "Int"
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
    let p := fn.getPrefix
    let s := match fn with | .str _ str => str | _ => ""
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
    if p == ``Bool then
      return "Boolean"
    if args.isEmpty then
      if let some d := (← read).declMap[fn]? then
        if let some d0 := isInlinedConstDecl? d then
          if let .lit v := d0.value then
            let effTy := if decl.type.isScalar then decl.type else d0.type
            return litKotlinType v effTy
    if fn == arrOp "get!Internal" || fn == arrOp "get!InternalBorrowed" then
      let arrTy ← arrayType (args[2]?.getD .erased)
      return (kotlinArrayElem? arrTy).getD baseTy
    if fn == arrOp "getInternal" || fn == arrOp "getInternalBorrowed" ||
       fn == arrOp "uget" || fn == arrOp "ugetBorrowed" then
      let arrTy ← arrayType (args[1]?.getD .erased)
      return (kotlinArrayElem? arrTy).getD baseTy
    if fn == arrOp "size" || fn == arrOp "usize" then
      return "Int"
    if baseTy != "Any?" then return baseTy
    fnRetKotlinType fn decl.type
  | _ => return baseTy

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
  | .fap fn args =>
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
      if extStr == "kotlin_op:id" && args.size == 1 then
        setParamVarName x (← toKotlinArg args[0]!)
        return true
      if extStr == "kotlin_op:cast" && args.size == 1 then
        let a0 ← toKotlinArg args[0]!
        let targetTy := toKotlinType decl.type
        if targetTy == "Any?" || (← get).varTypes[a0]? == some targetTy then
          setParamVarName x a0
          return true
    if args.size == 1 then
      let a0 ← toKotlinArg args[0]!
      if (← get).varTypes[a0]? == some "Int" then
        match fn with
        | ``UInt32.toNat | ``USize.toNat | ``UInt32.ofNat | ``USize.ofNat
        | ``Int32.ofNat | ``Int32.ofInt | ``ISize.ofNat | ``ISize.ofInt
        | ``Int.toNat | ``Int8.toInt | ``Int16.toInt | ``Int32.toInt
        | ``Int.toInt32 | ``Int.toInt64
        | ``Int32.toNatClampNeg | ``Int32.toISize | ``ISize.toInt32 =>
          setParamVarName x a0
          return true
        | _ => pure ()
      if (← get).varTypes[a0]? == some "UInt" then
        match fn with
        | ``UInt32.ofNat =>
          setParamVarName x a0
          return true
        | _ => pure ()
      if (← get).varTypes[a0]? == some "Long" then
        match fn with
        | ``Int64.ofNat | ``Int64.ofInt =>
          setParamVarName x a0
          return true
        | _ => pure ()
      if (← get).varTypes[a0]? == some "ULong" then
        match fn with
        | ``UInt64.ofNat =>
          setParamVarName x a0
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
  let ty := toKotlinType decl.type
  if (← classStructOf? y).isSome then
    let n ← getVarName x
    recordVarType n ty
    modify fun st => { st with projSrc := st.projSrc.insert n y }
    unless ← isUsed x do return
    let lhs ← classField y pos
    emitLn s!"val {n} = {lhs}"
    modify fun st => { st with fieldVals := st.fieldVals.insert lhs n }
  else
    let n ← getVarName x
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
    if cs.discr == x && cs.alts.size == 2 then 1
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

def isPrimitiveOp (fn : Name) (numArgs : Nat) : Bool :=
  let p := fn.getPrefix
  let s := match fn with | .str _ str => str | _ => ""
  if p == ``Int8 || p == ``Int16 || p == ``Int32 || p == ``Int64 || p == ``ISize ||
     p == ``UInt8 || p == ``UInt16 || p == ``UInt32 || p == ``UInt64 || p == ``USize then
    if numArgs == 1 then
      s.startsWith "to" || s.startsWith "of" || s == "neg" || s == "complement"
    else if numArgs == 2 then
      s == "add" || s == "sub" || s == "mul" || s == "div" || s == "mod" ||
      s == "land" || s == "lor" || s == "xor" || s == "lxor" ||
      s == "shiftLeft" || s == "shiftRight" ||
      s == "decEq" || s == "decLt" || s == "decLe"
    else false
  else if p == ``Nat || p == ``Int then
    if numArgs == 1 then s.startsWith "to" || s.startsWith "of" || s == "shiftLeft" || s == "add" || s == "sub" || s == "mul" || s == "div" || s == "mod"
    else if numArgs == 2 then
      s == "decEq" || s == "decLt" || s == "decLe" || s == "shiftLeft" || s == "add" || s == "sub" || s == "mul" || s == "div" || s == "mod"
    else false
  else if p == ``Bool then
    if numArgs == 1 then s.startsWith "to"
    else if numArgs == 2 then s == "decEq"
    else false
  else
    if numArgs == 1 && fn.isStr && (s == "ofNat" || s == "toUInt64" || s == "toUInt32") then true
    else false

def isPureLet (decl : LetDecl .impure) : EmitM Bool := do
  let .fap fn args := decl.value | return false
  if fn.isStr && (fn.getString!.startsWith "instInhabited" || fn.getString!.contains "inhabited" || fn.getString!.contains "boxed_const") then return true
  if (← loopExpandable? decl.value).isSome then return false
  if Ownership.isArraySet fn then return false
  if (Ownership.inplaceArg? (← getEnv) fn args).isSome then return false
  if (← dropShape? fn).isSome then return false
  if isPrimitiveOp fn args.size then return true
  if fn == arrOp "size" || fn == arrOp "usize" ||
     fn == arrOp "get!Internal" || fn == arrOp "get!InternalBorrowed" ||
     fn == arrOp "getInternal" || fn == arrOp "getInternalBorrowed" ||
     fn == arrOp "uget" || fn == arrOp "ugetBorrowed" then
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
    | ``UInt32.toNat | ``USize.toNat | ``UInt32.ofNat | ``USize.ofNat => return true
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
  | .sset y _ _ z _ _ _ | .uset y _ z _ _ =>
    return z == x && y != x
  | .let d2 k2 =>
    if (← loopExpandable? d2.value).isSome || (← papBeta? d2.value).isSome || (d2.value matches .pap ..) then
      return false
    let u := countUsesLetValue x d2.value
    if u > 0 then
      if u != 1 then return false
      match d2.value with
      | .box .. | .unbox .. =>
        return countUsesCode d2.fvarId k2 == 1 && (← usedBeforeSideEffect d2.fvarId k2)
      | .ctor info _ =>
        return info.name == ``Prod.mk && (skipRC k2 == .return d2.fvarId)
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
        if !(← read).classStructs.values.contains info.name.getPrefix then
          usedBeforeSideEffect x k2
        else
          return false
      | .fap .. =>
        if ← isPureLet d2 then usedBeforeSideEffect x k2 else return false
      | _ => return false
  | _ => return false

def tryInlineSingleUseLet? (decl : LetDecl .impure) (k : Code .impure) : EmitM Bool := do
  unless ← isInlineableLet decl do return false
  unless countUsesCode decl.fvarId k == 1 do return false
  unless ← usedBeforeSideEffect decl.fvarId k do return false
  let rhs ← emitLetValue decl
  let ty ← inferLetKotlinType decl
  recordVarType rhs ty
  setParamVarName decl.fvarId rhs
  return true

mutual

partial def emitLetAndContinue (decl : LetDecl .impure) (k : Code .impure) : EmitM Unit := do
  if let some (callee, args) ← loopExpandable? decl.value then
    if let .let d2 k2 := skipRC k then
      if let .unbox f := d2.value then
        if f == decl.fvarId then
          let curExit ← currentExit
          if let .return r := skipRC k2 then
            if (r == d2.fvarId || ((← read).retUnit && (curExit matches .funcReturn))) &&
               !(curExit matches .inlineAlias _ | .assignOnly ..) then
              emitLoopExpansion callee args none
              return
          emitLoopExpansion callee args (some (d2.fvarId, d2.type))
          setParamVarName decl.fvarId (← getVarName d2.fvarId)
          emitCode k2
          return
    emitLoopExpansion callee args (some (decl.fvarId, decl.type))
  else if let some (lam, args) ← papBeta? decl.value then
    -- Closures return boxed values; fuse `let y := unbox x` so the inlined body yields `y` directly.
    if let .let d2 k2 := skipRC k then
      if let .unbox f := d2.value then
        if f == decl.fvarId then
          let curExit ← currentExit
          if let .return r := skipRC k2 then
            if (r == d2.fvarId || ((← read).retUnit && (curExit matches .funcReturn))) &&
               !(curExit matches .inlineAlias _ | .assignOnly ..) then
              emitLoopExpansion lam args none
              return
          emitLoopExpansion lam args (some (d2.fvarId, d2.type))
          setParamVarName decl.fvarId (← getVarName d2.fvarId)
          emitCode k2
          return
    emitLoopExpansion lam args (some (decl.fvarId, decl.type))
  else if ← recordPap? decl then
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
        let n ← getVarName x
        modify fun st => { st with tuples := st.tuples.insert n strs }
      else if let some s := (← read).classStructs.values.find? (· == info.name.getPrefix) then
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
        let some y := src?
          | throwError "Kotlin backend: in `{(← read).currFn}`: cannot allocate a new `{s}` (`@[kotlin_class]` values are only updated in place)"
        for h : k in [:args.size] do
          if let .fvar v := args[k] then
            if ← isWriteBack x v then continue
          emitFieldWrite y (·.objs[k]?) (← toKotlinArg args[k])
        setParamVarName x (← getVarName y)
      else
        let n ← getVarName x
        recordVarType n (← inferLetKotlinType decl)
        emitLn s!"val {n} = {← emitLetValue decl}"
    | .oproj i y =>
      match (← get).tuples[← getVarName y]? with
      | some comps => setParamVarName x (comps[i]?.getD "null")
      | none => emitFieldRead x y (·.objs[i]?) decl
    | .sproj _ off y => emitFieldRead x y (·.scalars[off]?) decl
    | .uproj i y => emitFieldRead x y (·.usizes[i]?) decl
    | .isShared _ =>
      -- Verified by the ownership analysis: values updated in place are exclusively owned.
      setParamVarName x "false"
    | .reset _ y => setParamVarName x (← getVarName y)
    | .reuse y _ _ args =>
      for h : k in [:args.size] do
        emitFieldWrite y (·.objs[k]?) (← toKotlinArg args[k])
      setParamVarName x (← getVarName y)
    | .fap fn args =>
      if let some (a, idx, v) ← arraySet? fn args then
        let arrTy ← arrayType a
        let vStr ← toKotlinArg v
        let vStr ← match kotlinArrayElem? arrTy with
          | some e => castIfNeeded vStr e
          | none => pure vStr
        let aStr ← toKotlinArg a
        emitLn s!"{aStr}[{idx}] = {vStr}"
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
          let rhs ← emitLetValue decl
          let ty ← inferLetKotlinType decl
          if !(← isUsed x) && ty == "Unit" then
            emitLn rhs
          else
            let n ← getVarName x
            recordVarType n ty
            emitLn s!"val {n} = {rhs}"
    | _ =>
      let n ← getVarName x
      recordVarType n (← inferLetKotlinType decl)
      emitLn s!"val {n} = {← emitLetValue decl}"
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
          emitLoopExpansion callee args none
          return
        if let some (lam, args) ← papBeta? decl.value then
          emitLoopExpansion lam args none
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
              pure ((← dropShape? fn).isSome || Ownership.isArraySet fn ||
                Ownership.isInplaceExtern (← getEnv) fn)
            | .ctor info _ => pure (info.name == ``Prod.mk)
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
    if cs.alts.size == 2 then
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
      emitIndent; emit s!"when ({discrName}) "; emitLn "{"
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
      for p in params do
        if let some r := aliases[p.fvarId]? then
          setParamVarName p.fvarId (← getVarName r)
          isAliased := isAliased.push true
        else
          let pName ← getVarName p.fvarId
          let pType := toKotlinType p.type
          recordVarType pName pType
          emitLn s!"val {pName}: {pType}"
          isAliased := isAliased.push false
      let lbl? ← if useLbl then
        let c := (← get).loopCounter + 1
        modify fun st => { st with loopCounter := c }
        pure (some s!"jp_{c}")
      else
        pure none
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
    (dest? : Option (FVarId × Expr)) : EmitM Unit := do
  let callee ← callee.internalize (uniqueIdents := true)
  let .code body := callee.value | return
  markUsed body
  let n := (← get).loopCounter + 1
  modify fun st => { st with loopCounter := n }
  let params := callee.params
  let selfArgs := collectSelfCallArgs callee.name body #[]
  let aliases ← collectAliases body
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
        -- Loop-invariant parameter: alias the argument, no copy and no per-iteration update.
        setParamVarName p.fvarId a
        if ((← get).varTypes[a]?.getD "Any?") == "Any?" && pTy != "Any?" then
          recordVarType a pTy
        names := names.push a
    return names
  match dest? with
  | none =>
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
  | some (fv, ty) =>
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
  modify fun st => { st with varNames := {}, nameCounter := 0, inlinedJps := {}, blockJps := {}, knownBools := initKnownBools, loopCounter := 0, paps := {}, tuples := {}, nameStructs := {}, fieldVals := {}, used := {}, projSrc := {}, writeBacks := {}, varTypes := initVarTypes, aliases := {}, loopExits := {} }
  let .code code := decl.value | return ()
  markUsed code
  let fnAliases ← collectAliases code
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
  let numEmitted := if member?.isSome then params.size - 1 else params.size
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
  let retType := if retUnit then "Unit" else retCast?.getD (toKotlinType decl.type)
  let mut paramDecls : Array String := #[]
  for i in [:params.size] do
    let p := params[i]!
    if i == 0 && member?.isSome then
      setParamVarName p.fvarId "this"
      if let some info := member? then
        recordVarType "this" info.recvType
    else
      -- Parameter names are part of the Kotlin API (named arguments, API files), so keep the
      -- binder name when it is a plain identifier. Locals always get a numeric suffix.
      let raw := (origParamNames[i]?.getD p.binderName).eraseMacroScopes.toString
      let pName :=
        if !raw.isEmpty && raw.all (fun c => c.isAlphanum || c == '_') && !(raw.front.isDigit) then raw
        else s!"p_{i}"
      setParamVarName p.fvarId pName
      let emittedIdx := if member?.isSome then i - 1 else i
      let pType := (override? emittedIdx).getD (toKotlinType p.type)
      recordVarType pName pType
      paramDecls := paramDecls.push s!"{pName}: {pType}"
  let paramStr := String.intercalate ", " paramDecls.toList
  let codeJP := hasRecursiveJP code
  let isTailRec := !codeJP && hasSelfTailCall decl.name code
  let isInline := !codeJP && !hasSelfCall decl.name code && Compiler.hasInlineAttribute env decl.name
  let (mods, fnName) := match member? with
    | some info => (info.modifiers, memberKotlinName decl.name info)
    | none =>
      let topMods := compiler.kotlin.topLevelModifiers.get opts
      let topMods := if topMods == "internal" then "@PublishedApi internal" else topMods
      (topMods, toKotlinFnName decl.name)
  let explicitInline := (identTokens mods).contains "inline"
  let auto :=
    if isTailRec then "tailrec "
    else if isInline && !explicitInline then "inline "
    else ""
  let modsStr := if mods.isEmpty then "" else mods ++ " "
  if let some (.inl doc) ← findInternalDocString? env decl.name (includeBuiltin := false) then
    for l in kdocLines doc do emitLn l
  emitIndent; emit s!"{modsStr}{auto}fun {fnName}({paramStr}): {retType} "; emitLn "{"
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

public def emitKotlinForDecls (modName : Name) (decls : Array Name) : CoreM String := do
  let (localDecls, otherModuleDecls) ← collectUsedDecls decls
  let env ← getEnv
  let opts ← getOptions
  let indexMap := getImpureDeclIndices env decls
  let localDecls := localDecls.qsort fun l r => indexMap[l.name]! < indexMap[r.name]!
  let declMap := localDecls.foldl (init := ({} : Std.HashMap Name (Decl .impure))) fun m d => m.insert d.name d
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
  let toEmit ←
    if !compiler.kotlin.pruneUnreachable.get opts then
      pure localDecls
    else
      let tokens := identTokens (headerText ++ "\n" ++ footerText ++ "\n" ++ specText)
      let roots := localDecls.filter fun d =>
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
              for m in collectCalls c #[] do
                if declMap.contains m && !visited.contains m then
                  work := work.push m
              for m in collectCalls (paps := false) c #[] do
                applied := applied.insert m
      let rootNames := roots.map (·.name)
      -- Loop-expandable declarations are emitted inline at every call site, and declarations
      -- that are only partially applied are beta-inlined at every application.
      pure <| localDecls.filter fun d =>
        visited.contains d.name && !isLoopExpandable env d && (isInlinedConstDecl? d).isNone &&
          (applied.contains d.name || rootNames.contains d.name)
  let classStructs := env.constants.map₂.foldl (init := ({} : Std.HashMap String Name)) fun m n _ =>
    match Compiler.getKotlinClass? env n with
    | some t => m.insert t n
    | none => m
  -- In-place updates must be justified by exclusive ownership.
  let papTargets := localDecls.foldl (init := ({} : Std.HashSet Name)) fun acc d =>
    match d.value with
    | .code c => collectPaps c acc
    | _ => acc
  let isMember (n : Name) := (Compiler.getKotlinMemberInfo? env n).isSome
  let ownership ← (Ownership.analyze localDecls isMember
    (fun n => isMember n || isExport env n || papTargets.contains n)).run (phase := .impure)
  unless ownership.errors.isEmpty do
    throwError (MessageData.joinSep ownership.errors.toList "\n")
  let ctx : Context := { modName, localDecls, otherModuleDecls, declMap,
                         summaries := ownership.summaries, classStructs }
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
      for cls in memberBufs.keys do
        unless usedH.contains cls || usedF.contains cls do
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
