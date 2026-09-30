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

/-- State of a self-tail-recursive function whose body is being emitted as a `while (true)` loop. -/
structure LoopCtx where
  fnName : Name
  /-- Kotlin names holding the current value of each parameter. -/
  varNames : Array String
  /-- `true` if the parameter changes across iterations (emitted as `var`). -/
  variant : Array Bool
  loopLbl : String
  /-- `none`: results are returned from the enclosing Kotlin function; `some l`: `return@l`. -/
  retLbl? : Option String

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

structure State where
  buf : String := ""
  indent : Nat := 0
  varNames : Std.HashMap FVarId String := {}
  nameCounter : Nat := 0
  inlinedJps : Std.HashMap FVarId (FunDecl .impure) := {}
  /-- Boolean variables (by Kotlin name) whose value is known in the current branch. -/
  knownBools : Std.HashMap String Bool := {}
  loopCounter : Nat := 0

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

def toKotlinType (ty : Expr) : String :=
  match ty with
  | ImpureType.uint8 => "Boolean"
  | ImpureType.uint16 => "Short"
  | ImpureType.uint32 => "Int"
  | ImpureType.uint64 => "Long"
  | ImpureType.usize => "Int"
  | ImpureType.float => "Double"
  | ImpureType.float32 => "Float"
  | ImpureType.void => "Unit"
  | .const ``Bool _ => "Boolean"
  | .const ``Unit _ => "Unit"
  | .app (.const `jvmType _) (.lit (.strVal desc)) =>
    if desc.startsWith "kotlin:" then
      (desc.drop 7).toString
    else match desc with
    | "Ljava/lang/Object;" => "Any?"
    | "Ljava/lang/String;" => "String"
    | "[Z" => "BooleanArray"
    | "[B" => "ByteArray"
    | "[C" => "CharArray"
    | "[S" => "ShortArray"
    | "[I" => "IntArray"
    | "[J" => "LongArray"
    | "[F" => "FloatArray"
    | "[D" => "DoubleArray"
    | "[Ljava/lang/Object;" => "Array<Any?>"
    | _ =>
      if desc.startsWith "[L" && desc.endsWith ";" then
        let elem := (desc.drop 2 |>.dropEnd 1).toString.replace "/" "."
        s!"Array<{elem}>"
      else if desc.startsWith "L" && desc.endsWith ";" then
        (desc.drop 1 |>.dropEnd 1).toString.replace "/" "."
      else
        "Any?"
  | _ => "Any?"

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
  let cleanName := name.replace "." "_"
  modify fun st => { st with varNames := st.varNames.insert fvarId cleanName }

def toKotlinFnName (n : Name) : String :=
  n.mangle (pre := "f_")

def toKotlinArg (arg : Arg .impure) : EmitM String := do
  match arg with
  | .fvar fvarId => getVarName fvarId
  | .erased => return "null"

def formatUInt32 (n : UInt32) : String :=
  let signedVal : Int := if n.toNat > 2147483647 then (n.toNat : Int) - 4294967296 else (n.toNat : Int)
  s!"{signedVal}"

def formatUInt64 (n : UInt64) : String :=
  let signedVal : Int := if n.toNat > 9223372036854775807 then (n.toNat : Int) - 18446744073709551616 else (n.toNat : Int)
  s!"{signedVal}L"

def splitCharOnce (cs : List Char) (sep : Char) : Option (String × String) :=
  let rec go (acc : List Char) : List Char → Option (String × String)
    | [] => none
    | c :: rest =>
      if c == sep then
        some (String.ofList acc.reverse, String.ofList rest)
      else
        go (c :: acc) rest
  go [] cs

def parseJvmMemberSpec? (spec : String) : Option (String × String × String) := do
  let (lhs, desc) ← splitCharOnce spec.toList ':'
  let (className, memberName) ← splitCharOnce lhs.toList '.'
  return (className, memberName, desc)

def emitPrimitiveOp? (fn : Name) (args : Array (Arg .impure)) : EmitM (Option String) := do
  if args.size == 1 then
    let a0 ← toKotlinArg args[0]!
    if fn.isStr && (fn.getString! == "ofNat" || fn.getString! == "toUInt64") then
      return some s!"({a0} as Number).toLong()"
    if fn.isStr && fn.getString! == "toUInt32" then
      return some s!"({a0} as Number).toInt()"
    match fn with
    | `UInt32.toUInt64 | `UInt32.toUSize =>
      return some s!"({a0}.toLong() and 0xffffffffL)"
    | `UInt64.toUInt32 | `USize.toUInt32 =>
      return some s!"{a0}.toInt()"
    | `UInt8.toUInt32 | `UInt16.toUInt32 =>
      return some s!"{a0}.toInt()"
    | `UInt32.toUInt8 =>
      return some s!"{a0}.toByte()"
    | `UInt32.toUInt16 =>
      return some s!"{a0}.toShort()"
    | `UInt8.toUInt64 | `UInt16.toUInt64 =>
      return some s!"{a0}.toLong()"
    | `UInt64.toUInt8 =>
      return some s!"{a0}.toByte()"
    | `UInt64.toUInt16 =>
      return some s!"{a0}.toShort()"
    | _ => return none
  if args.size == 2 then
    let a0 ← toKotlinArg args[0]!
    let a1 ← toKotlinArg args[1]!
    match fn with
    | ``UInt32.add | ``UInt64.add | ``USize.add =>
      return some s!"({a0} + {a1})"
    | ``UInt32.sub | ``UInt64.sub | ``USize.sub =>
      return some s!"({a0} - {a1})"
    | ``UInt32.mul | ``UInt64.mul | ``USize.mul =>
      return some s!"({a0} * {a1})"
    | ``UInt32.div | ``UInt64.div | ``USize.div =>
      return some s!"({a0} / {a1})"
    | ``UInt32.mod | ``UInt64.mod | ``USize.mod =>
      return some s!"({a0} % {a1})"
    | ``UInt32.land | ``UInt64.land | ``USize.land =>
      return some s!"({a0} and {a1})"
    | ``UInt32.lor | ``UInt64.lor | ``USize.lor =>
      return some s!"({a0} or {a1})"
    | ``UInt32.xor | `UInt32.lxor | ``UInt64.xor | `UInt64.lxor | ``USize.xor | `USize.lxor =>
      return some s!"({a0} xor {a1})"
    | ``UInt32.shiftLeft | ``UInt64.shiftLeft | ``USize.shiftLeft =>
      return some s!"({a0} shl ({a1}).toInt())"
    | ``UInt32.shiftRight | ``UInt64.shiftRight | ``USize.shiftRight =>
      return some s!"({a0} ushr ({a1}).toInt())"
    | ``UInt32.decEq | ``UInt64.decEq =>
      return some s!"({a0} == {a1})"
    | ``UInt32.decLt | ``UInt64.decLt =>
      return some s!"({a0} < {a1})"
    | ``UInt32.decLe | ``UInt64.decLe =>
      return some s!"({a0} <= {a1})"
    | `Nat.shiftLeft =>
      return some s!"(({a0} as Int) shl ({a1}).toInt())"
    | `Nat.add =>
      return some s!"(({a0} as Int) + ({a1} as Int))"
    | `Nat.sub =>
      return some s!"(maxOf(0, ({a0} as Int) - ({a1} as Int)))"
    | `Nat.mul =>
      return some s!"(({a0} as Int) * ({a1} as Int))"
    | `Nat.div =>
      return some s!"(({a0} as Int) / ({a1} as Int))"
    | `Nat.mod =>
      return some s!"(({a0} as Int) % ({a1} as Int))"
    | _ => return none
  return none

def emitExternCall? (fn : Name) (args : Array (Arg .impure)) : EmitM (Option String) := do
  let env ← getEnv
  let some extStr := getExternNameFor env `jvm fn | return none
  if extStr.startsWith "jvm_op:" then
    let op := (extStr.drop 7).toString
    match op with
    | "u32_to_u64" =>
      let a0 ← toKotlinArg args[0]!
      return some s!"({a0}.toLong() and 0xffffffffL)"
    | "u64_to_u32" | "l2i" =>
      let a0 ← toKotlinArg args[0]!
      return some s!"{a0}.toInt()"
    | "i2l" =>
      let a0 ← toKotlinArg args[0]!
      return some s!"{a0}.toLong()"
    | "lshr" =>
      let a0 ← toKotlinArg args[0]!
      let a1 ← toKotlinArg args[1]!
      return some s!"({a0} shr ({a1}).toInt())"
    | "lushr" =>
      let a0 ← toKotlinArg args[0]!
      let a1 ← toKotlinArg args[1]!
      return some s!"({a0} ushr ({a1}).toInt())"
    | "lshl" =>
      let a0 ← toKotlinArg args[0]!
      let a1 ← toKotlinArg args[1]!
      return some s!"({a0} shl ({a1}).toInt())"
    | "if_icmplt" =>
      let a0 ← toKotlinArg args[0]!
      let a1 ← toKotlinArg args[1]!
      return some s!"({a0} < {a1})"
    | "if_icmpge" =>
      let a0 ← toKotlinArg args[0]!
      let a1 ← toKotlinArg args[1]!
      return some s!"({a0} >= {a1})"
    | "if_icmpgt" =>
      let a0 ← toKotlinArg args[0]!
      let a1 ← toKotlinArg args[1]!
      return some s!"({a0} > {a1})"
    | "if_icmple" =>
      let a0 ← toKotlinArg args[0]!
      let a1 ← toKotlinArg args[1]!
      return some s!"({a0} <= {a1})"
    | "if_acmpeq" =>
      let a0 ← toKotlinArg args[0]!
      let a1 ← toKotlinArg args[1]!
      return some s!"({a0} === {a1})"
    | "ifnull" =>
      let a0 ← toKotlinArg args[0]!
      return some s!"({a0} == null)"
    | "aconst_null" =>
      return some "null"
    | "laload" | "aaload" =>
      let a0 ← toKotlinArg args[0]!
      let a1 ← toKotlinArg args[1]!
      return some s!"{a0}[({a1}).toInt()]"
    | "lastore" | "aastore" =>
      let a0 ← toKotlinArg args[0]!
      let a1 ← toKotlinArg args[1]!
      let a2 ← toKotlinArg args[2]!
      return some ("run { " ++ a0 ++ "[(" ++ a1 ++ ").toInt()] = " ++ a2 ++ "; Unit }")
    | "arraylength" =>
      let a0 ← toKotlinArg args[0]!
      return some s!"{a0}.size"
    | "newarray_long" =>
      let a0 ← toKotlinArg args[0]!
      return some s!"LongArray(({a0}).toInt())"
    | "anewarray_object" =>
      let a0 ← toKotlinArg args[0]!
      return some s!"arrayOfNulls<Any?>(({a0}).toInt())"
    | "obj_array_fill_null" =>
      let a0 ← toKotlinArg args[0]!
      let a1 ← toKotlinArg args[1]!
      let a2 ← toKotlinArg args[2]!
      return some ("run { java.util.Arrays.fill(" ++ a0 ++ " as Array<Any?>, (" ++ a1 ++ ").toInt(), (" ++ a2 ++ ").toInt(), null); Unit }")
    | _ => return none
  if extStr.startsWith "invokestatic:" then
    let rest := (extStr.drop 13).toString
    if rest.startsWith "java/lang/Long.numberOfTrailingZeros:(J)I" then
      let a0 ← toKotlinArg args[0]!
      return some s!"{a0}.countTrailingZeroBits()"
    if rest.startsWith "java/lang/Integer.numberOfLeadingZeros:(I)I" then
      let a0 ← toKotlinArg args[0]!
      return some s!"{a0}.countLeadingZeroBits()"
    if rest.startsWith "java/util/Arrays.fill:([JJ)V" then
      let a0 ← toKotlinArg args[0]!
      let a1 ← toKotlinArg args[1]!
      return some ("run { " ++ a0 ++ ".fill(" ++ a1 ++ "); Unit }")
    if rest.startsWith "java/util/Objects.hashCode:(Ljava/lang/Object;)I" then
      let a0 ← toKotlinArg args[0]!
      return some s!"({a0}?.hashCode() ?: 0)"
    if rest.startsWith "java/util/Objects.equals:(Ljava/lang/Object;Ljava/lang/Object;)Z" then
      let a0 ← toKotlinArg args[0]!
      let a1 ← toKotlinArg args[1]!
      return some s!"({a0} == {a1})"
    return none
  if extStr.startsWith "getfield:" then
    let spec := (extStr.drop 9).toString
    let a0 ← toKotlinArg args[0]!
    let memberName := (parseJvmMemberSpec? spec).map (·.2.1) |>.getD spec
    return some (if a0 == "this" then memberName else s!"{a0}.{memberName}")
  if extStr.startsWith "putfield:" then
    let spec := (extStr.drop 9).toString
    let a0 ← toKotlinArg args[0]!
    let a1 ← toKotlinArg args[1]!
    let memberName := (parseJvmMemberSpec? spec).map (·.2.1) |>.getD spec
    let lhs := if a0 == "this" then memberName else s!"{a0}.{memberName}"
    return some s!"run \{ {lhs} = {a1}; Unit }"
  if extStr.startsWith "getstatic:" then
    let spec := (extStr.drop 10).toString
    if let some (className, memberName, _) := parseJvmMemberSpec? spec then
      if className.endsWith "Kt" then
        return some memberName
      else
        let owner := className.replace "/" "."
        return some s!"{owner}.{memberName}"
    else
      return some spec
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
    | some t => if t == "_" then pure s else pure s!"({s} as {t})"
    | none => pure s
  let call := s!"{memberKotlinName fn info}({String.intercalate ", " rest.toList})"
  if recv == "this" then
    if (← read).currClass? == some info.className then
      return some call
    return some s!"(this as {info.recvType}).{call}"
  let sameType ← match args[0]! with
    | .fvar f => do pure (toKotlinType (← getType f) == info.recvType)
    | .erased => pure false
  if sameType then
    return some s!"{recv}.{call}"
  return some s!"({recv} as {info.recvType}).{call}"

def emitLetValue (decl : LetDecl .impure) : EmitM String := do
  match decl.value with
  | .lit v =>
    match v with
    | .uint8 n => return if n == 0 then "false" else "true"
    | .uint16 n => return s!"{n}.toShort()"
    | .uint32 n => return formatUInt32 n
    | .usize n => return formatUInt32 n.toUInt32
    | .uint64 n => return formatUInt64 n
    | .nat n =>
      if n > 2147483647 then
        let signedVal : Int := (n : Int) - 4294967296
        return s!"{signedVal}"
      else
        return s!"{n}"
    | .str s => return s!"\"{s}\""
  | .erased => return "null"
  | .fap fn args =>
    if let some expr ← emitPrimitiveOp? fn args then
      return expr
    if let some expr ← emitExternCall? fn args then
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
    return s!"({← getVarName fvarId} as {toKotlinType decl.type})"
  | _ => return "null"

/-- Continuation of RC and field-update instructions, which the Kotlin backend ignores. -/
def skipCont? : Code .impure → Option (Code .impure)
  | .del (k := k) .. | .dec (k := k) .. | .inc (k := k) .. | .setTag (k := k) ..
  | .sset (k := k) .. | .uset (k := k) .. | .oset (k := k) .. => some k
  | _ => none

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

partial def hasSelfTailCall (fnName : Name) (code : Code .impure) : Bool :=
  match code with
  | .let decl k =>
    match k with
    | .return fvarId => decl.fvarId == fvarId && isCallTo fnName decl.value
    | _ => hasSelfTailCall fnName k
  | .cases c => c.alts.any fun alt => hasSelfTailCall fnName alt.getCode
  | c => match skipCont? c with | some k => hasSelfTailCall fnName k | none => false

partial def hasMultiUseJP (code : Code .impure) : Bool :=
  match code with
  | .let _ k => hasMultiUseJP k
  | .jp decl k =>
    if countJmp decl.fvarId k > 2 then true
    else hasMultiUseJP decl.value || hasMultiUseJP k
  | .cases c => c.alts.any fun alt => hasMultiUseJP alt.getCode
  | c => match skipCont? c with | some k => hasMultiUseJP k | none => false

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
      match k with
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

/-- Declarations called or partially applied in `code`. -/
partial def collectCalls (code : Code .impure) (acc : Array Name) : Array Name :=
  match code with
  | .let decl k =>
    let acc := match decl.value with
      | .fap n _ | .pap n _ => acc.push n
      | _ => acc
    collectCalls k acc
  | .jp decl k => collectCalls k (collectCalls decl.value acc)
  | .cases c => c.alts.foldl (fun acc alt => collectCalls alt.getCode acc) acc
  | c => match skipCont? c with | some k => collectCalls k acc | none => acc

/--
An `@[inline]` self-tail-recursive function (all self-calls in tail position, no multi-use join
points) is emitted as a `while (true)` loop at each call site instead of being called.
-/
def isLoopExpandable (env : Environment) (decl : Decl .impure) : Bool :=
  match decl.value with
  | .code code =>
    Compiler.hasInlineAttribute env decl.name &&
    (Compiler.getKotlinMemberInfo? env decl.name).isNone &&
    hasSelfCall decl.name code && selfCallsAllTail decl.name code && !hasMultiUseJP code
  | _ => false

def loopExpandable? (v : LetValue .impure) : EmitM (Option (Decl .impure × Array (Arg .impure))) := do
  let .fap fn args := v | return none
  if let some l := (← read).loop? then
    if l.fnName == fn then return none
  let some d := (← read).declMap[fn]? | return none
  if isLoopExpandable (← getEnv) d then return some (d, args) else return none

def emitReturn (valStr : String) : EmitM Unit := do
  match (← read).loop?.bind (·.retLbl?) with
  | some l => emitLn s!"return@{l} {valStr}"
  | none =>
    match (← read).retCast? with
    | some t => emitLn s!"return ({valStr} as {t})"
    | none => emitLn s!"return {valStr}"

def withKnown (name : String) (b : Bool) (act : EmitM Unit) : EmitM Unit := do
  let saved := (← get).knownBools
  modify fun st => { st with knownBools := st.knownBools.insert name b }
  act
  modify fun st => { st with knownBools := saved }

/-- Self tail call inside a loop-expanded body: update the loop variables and `continue`. -/
def emitLoopContinue (l : LoopCtx) (args : Array (Arg .impure)) : EmitM Unit := do
  let mut updates : Array (String × String) := #[]
  for i in [:l.varNames.size] do
    if l.variant[i]! then
      let a ← toKotlinArg (args[i]?.getD .erased)
      if a != l.varNames[i]! then
        updates := updates.push (l.varNames[i]!, a)
  let targets := updates.map (·.1)
  let needTemps := updates.any fun (_, a) => targets.contains a
  if needTemps then
    for (v, a) in updates do emitLn s!"val {v}_t = {a}"
    for (v, _) in updates do emitLn s!"{v} = {v}_t"
  else
    for (v, a) in updates do emitLn s!"{v} = {a}"
  emitLn s!"continue@{l.loopLbl}"

mutual

partial def emitLetAndContinue (decl : LetDecl .impure) (k : Code .impure) : EmitM Unit := do
  if let some (callee, args) ← loopExpandable? decl.value then
    emitLoopExpansion callee args (some (decl.fvarId, decl.type))
  else
    let varName ← getVarName decl.fvarId
    let valStr ← emitLetValue decl
    emitLn s!"val {varName} = {valStr}"
  emitCode k

partial def emitCode (code : Code .impure) : EmitM Unit := do
  match code with
  | .let decl k =>
    match k with
    | .return fvarId =>
      if fvarId == decl.fvarId then
        if let some l := (← read).loop? then
          if let .fap fn args := decl.value then
            if fn == l.fnName then
              emitLoopContinue l args
              return
        if let some (callee, args) ← loopExpandable? decl.value then
          emitLoopExpansion callee args none
          return
        let valStr ← emitLetValue decl
        emitReturn valStr
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
        let cond := if thenWhen then discrName else s!"!{discrName}"
        emitIndent; emit s!"if ({cond}) "; emitLn "{"
        withIndent (withKnown discrName thenWhen (emitCode thenCode))
        emitIndent; emit "} else "; emitLn "{"
        withIndent (withKnown discrName (!thenWhen) (emitCode elseCode))
        emitLn "}"
    else
      emitIndent; emit s!"when ({discrName}) "; emitLn "{"
      withIndent do
        for alt in cs.alts do
          match alt with
          | .ctorAlt info altCode =>
            emitIndent; emit s!"{info.cidx} -> "; emitLn "{"
            withIndent (emitCode altCode)
            emitLn "}"
          | .default altCode =>
            emitIndent; emit "else -> "; emitLn "{"
            withIndent (emitCode altCode)
            emitLn "}"
      emitLn "}"
  | .return fvarId =>
    emitReturn (← getVarName fvarId)
  | .jp decl k =>
    let useCount := countJmp decl.fvarId k
    if useCount <= 2 then
      modify fun st => { st with inlinedJps := st.inlinedJps.insert decl.fvarId decl }
      emitCode k
    else
      let jpName ← getVarName decl.fvarId
      let params := decl.params
      let retType := toKotlinType decl.type
      let paramDecls : Array String ← params.mapM fun p => do
        let pName ← getVarName p.fvarId
        let pType := toKotlinType p.type
        return s!"{pName}: {pType}"
      let paramStr := String.intercalate ", " paramDecls.toList
      emitIndent; emit s!"fun {jpName}({paramStr}): {retType} "; emitLn "{"
      -- A local function body returns from itself, not from any enclosing loop block.
      withIndent (withReader (fun ctx => { ctx with loop? := none, retCast? := none }) (emitCode decl.value))
      emitLn "}"
      emitCode k
  | .jmp fvarId args =>
    if let some decl := (← get).inlinedJps[fvarId]? then
      for (p, arg) in decl.params.zip args do
        let argStr ← toKotlinArg arg
        setParamVarName p.fvarId argStr
      emitCode decl.value
    else
      let fnName ← getVarName fvarId
      let argStrs ← args.mapM toKotlinArg
      let argStr := String.intercalate ", " argStrs.toList
      emitReturn s!"{fnName}({argStr})"
  | .unreach _ =>
    emitLn "error(\"unreachable\")"
  | c =>
    if let some k := skipCont? c then emitCode k

/--
Emits the body of the loop-expandable `callee` applied to `args` as a `while (true)` loop.
`dest? = none`: tail position, results go wherever the enclosing context returns to.
`dest? = some (x, ty)`: `val x = run<ty> blk@{ ... }`.
-/
partial def emitLoopExpansion (callee : Decl .impure) (args : Array (Arg .impure))
    (dest? : Option (FVarId × Expr)) : EmitM Unit := do
  let callee ← callee.internalize (uniqueIdents := true)
  let .code body := callee.value | return
  let n := (← get).loopCounter + 1
  modify fun st => { st with loopCounter := n }
  let params := callee.params
  let selfArgs := collectSelfCallArgs callee.name body #[]
  let variant := params.mapIdx fun i p => selfArgs.any fun as =>
    match as[i]? with
    | some (.fvar f) => f != p.fvarId
    | _ => true
  let outerRet := (← read).loop?.bind (·.retLbl?)
  let retLbl? := if dest?.isSome then some s!"blk_{n}" else outerRet
  let loopLbl := s!"loop_{n}"
  let emitLoop : EmitM Unit := do
    let mut names : Array String := #[]
    for i in [:params.size] do
      let p := params[i]!
      let a ← toKotlinArg (args[i]?.getD .erased)
      if variant[i]! then
        let nm ← getVarName p.fvarId
        emitLn s!"var {nm}: {toKotlinType p.type} = {a}"
        names := names.push nm
      else
        -- Loop-invariant parameter: alias the argument, no copy and no per-iteration update.
        setParamVarName p.fvarId a
        names := names.push a
    emitIndent; emit s!"{loopLbl}@ while (true) "; emitLn "{"
    let lc : LoopCtx := { fnName := callee.name, varNames := names, variant, loopLbl, retLbl? }
    withIndent (withReader (fun ctx => { ctx with loop? := some lc }) (emitCode body))
    emitLn "}"
    emitLn "throw IllegalStateException(\"unreachable\")"
  match dest? with
  | none => emitLoop
  | some (fv, ty) =>
    let x ← getVarName fv
    emitIndent; emit s!"val {x} = run<{toKotlinType ty}> blk_{n}@ "; emitLn "{"
    withIndent emitLoop
    emitLn "}"

end

def emitFnDecl (decl : Decl .impure) : EmitM Unit := do
  let decl ← decl.internalize (uniqueIdents := true)
  modify fun st => { st with varNames := {}, nameCounter := 0, inlinedJps := {}, knownBools := {}, loopCounter := 0 }
  let .code code := decl.value | return ()
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
  let retType := retCast?.getD (toKotlinType decl.type)
  let mut paramDecls : Array String := #[]
  for i in [:params.size] do
    let p := params[i]!
    if i == 0 && member?.isSome then
      setParamVarName p.fvarId "this"
    else
      let cleanName := p.binderName.toString.replace "." "_"
      let pName := s!"{cleanName}_{i}"
      setParamVarName p.fvarId pName
      let emittedIdx := if member?.isSome then i - 1 else i
      let pType := (override? emittedIdx).getD (toKotlinType p.type)
      paramDecls := paramDecls.push s!"{pName}: {pType}"
  let paramStr := String.intercalate ", " paramDecls.toList
  let codeJP := hasMultiUseJP code
  let isTailRec := !codeJP && hasSelfTailCall decl.name code
  let isInline := !codeJP && !hasSelfCall decl.name code && Compiler.hasInlineAttribute env decl.name
  let auto :=
    if isTailRec then "tailrec "
    else if isInline then "inline "
    else ""
  let (mods, fnName) := match member? with
    | some info => (info.modifiers, memberKotlinName decl.name info)
    | none => (compiler.kotlin.topLevelModifiers.get opts, toKotlinFnName decl.name)
  let modsStr := if mods.isEmpty then "" else mods ++ " "
  emitIndent; emit s!"{modsStr}{auto}fun {fnName}({paramStr}): {retType} "; emitLn "{"
  withIndent do
    withReader (fun ctx => { ctx with currFn := decl.name, currParams := params, currClass? := member?.map (·.className), retCast? }) do
      emitCode code
  emitLn "}"
  emitLn ""

def captureBuf (act : EmitM Unit) : EmitM String := do
  let saved := (← get).buf
  modify fun st => { st with buf := "" }
  act
  let out := (← get).buf
  modify fun st => { st with buf := saved }
  return out

def identTokens (s : String) : Std.HashSet String :=
  let (set, cur) := s.foldl (init := (({} : Std.HashSet String), "")) fun (set, cur) c =>
    if c.isAlphanum || c == '_' then (set, cur.push c)
    else (if cur.isEmpty then set else set.insert cur, "")
  if cur.isEmpty then set else set.insert cur

def splitLines (s : String) : List String :=
  let (acc, cur) := s.foldl (init := (([] : List String), "")) fun (acc, cur) c =>
    if c == '\n' then (cur :: acc, "") else (acc, cur.push c)
  (cur :: acc).reverse

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

public def emitKotlinForDecls (modName : Name) (decls : Array Name) : CoreM String := do
  let (localDecls, otherModuleDecls) ← collectUsedDecls decls
  let env ← getEnv
  let opts ← getOptions
  let indexMap := getImpureDeclIndices env decls
  let localDecls := localDecls.qsort fun l r => indexMap[l.name]! < indexMap[r.name]!
  let declMap := localDecls.foldl (init := ({} : Std.HashMap Name (Decl .impure))) fun m d => m.insert d.name d
  let readOpt (inline file : String) : CoreM String := do
    let fileContent ← if file.isEmpty then pure "" else IO.FS.readFile file
    return String.intercalate "\n\n" ([inline, fileContent].filter (!·.isEmpty))
  let headerText ← readOpt (compiler.kotlin.preamble.get opts) (compiler.kotlin.preamble_file.get opts)
  let footerText ← readOpt (compiler.kotlin.footer.get opts) (compiler.kotlin.footer_file.get opts)
  let toEmit ←
    if !compiler.kotlin.pruneUnreachable.get opts then
      pure localDecls
    else
      let tokens := identTokens (headerText ++ "\n" ++ footerText)
      let roots := localDecls.filter fun d =>
        (Compiler.getKotlinMemberInfo? env d.name).isSome || isExport env d.name ||
          tokens.contains (toKotlinFnName d.name)
      let mut visited : Std.HashSet Name := {}
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
      -- Loop-expandable declarations are emitted inline at every call site.
      pure <| localDecls.filter fun d => visited.contains d.name && !isLoopExpandable env d
  let ctx : Context := { modName, localDecls, otherModuleDecls, declMap }
  let ((), st) ← ((do
    let mut topBuf := ""
    let mut memberBufs : Std.HashMap String String := {}
    for d in toEmit do
      let s ← captureBuf (emitFnDecl d)
      match Compiler.getKotlinMemberInfo? env d.name with
      | some info => memberBufs := memberBufs.insert info.className (memberBufs.getD info.className "" ++ s)
      | none => topBuf := topBuf ++ s
    let (hdr, usedH) := spliceMembers headerText memberBufs
    let (ftr, usedF) := spliceMembers footerText memberBufs
    for cls in memberBufs.keys do
      unless usedH.contains cls || usedF.contains cls do
        throwError "Kotlin backend: no `// @LeanMembers({cls})` marker in the preamble or footer"
    emitLn "// Generated automatically by Lean 4 to Kotlin compiler backend."
    emitLn "// DO NOT EDIT DIRECTLY."
    emitLn "@file:Suppress(\"UNCHECKED_CAST\", \"UNUSED_VARIABLE\", \"NAME_SHADOWING\", \"RemoveRedundantBackticks\", \"ConstantConditionIf\", \"RedundantExplicitType\", \"RedundantCallOfConversionMethod\", \"USELESS_CAST\", \"NOTHING_TO_INLINE\", \"UNREACHABLE_CODE\")"
    let pkg := compiler.kotlin.package.get opts
    let pkgName :=
      if !pkg.isEmpty then pkg
      else
        let parts := modName.components
        if parts.length > 1 then
          String.intercalate "." (parts.dropLast.map (·.toString))
        else
          ""
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
