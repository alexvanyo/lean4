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
public import Lean.Compiler.JVM.ClassFile
public import Lean.Compiler.JVM.Opcode
public import Init.Data.ByteArray

namespace Lean.Compiler.LCNF

public section

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

namespace JVM

def leanObjTypeDesc := "Llean/runtime/LeanObject;"
def leanCtorTypeDesc := "Llean/runtime/LeanCtor;"
def leanStringTypeDesc := "Llean/runtime/LeanString;"
def leanNatTypeDesc := "Llean/runtime/LeanNat;"
def leanClosureTypeDesc := "Llean/runtime/LeanClosure;"
def leanRuntimeJVMClass := "lean/runtime/LeanRuntimeJVM"
def leanCtorClass := "lean/runtime/LeanCtor"
def leanNatClass := "lean/runtime/LeanNat"
def leanStringClass := "lean/runtime/LeanString"

def getScalarNumBytes (ty : Expr) : Nat :=
  match ty with
  | ImpureType.uint8 => 1
  | ImpureType.uint16 => 2
  | ImpureType.uint32 | ImpureType.float32 => 4
  | ImpureType.uint64 | ImpureType.usize | ImpureType.float => 8
  | _ => 8

def toJVMTypeDesc (type : Expr) : String :=
  match type with
  | ImpureType.uint8 | ImpureType.uint16 | ImpureType.uint32 => "I"
  | ImpureType.uint64 | ImpureType.usize => "J"
  | ImpureType.float => "D"
  | ImpureType.float32 => "F"
  | ImpureType.tagged => "I"
  | _ => leanObjTypeDesc

def getSlotSize (type : Expr) : UInt8 :=
  match toJVMTypeDesc type with
  | "J" | "D" => 2
  | _ => 1

def isScalarType (ty : Expr) : Bool :=
  match toJVMTypeDesc ty with
  | "I" | "J" | "F" | "D" => true
  | _ => false

def toJVMClassName (modName : Name) : String :=
  "lean/mod_" ++ modName.mangle

def toJVMMethodName (fnName : Name) : String :=
  fnName.mangle (pre := "f_")

structure JVMContext where
  modName          : Name
  className        : String
  localDecls       : Array (Decl .impure)
  otherModuleDecls : Array (Signature .impure)
  currFn           : Name := default
  currReturnType   : Expr := ImpureType.object
  currParams       : Array (Param .impure) := #[]
  varSlotMap       : Std.HashMap FVarId UInt8 := {}
  varTypeMap       : Std.HashMap FVarId Expr := {}
  nextSlot         : UInt8 := 0
  joinPoints       : Std.HashMap FVarId (FunDecl .impure) := {}

structure JVMState where
  cf : ClassFile
  code : ByteArray := ByteArray.empty
  labelOffsets : Std.HashMap Name Nat := {}
  fixups : Array (Nat × Name) := #[]
  /-- Synthetic closure classes generated for `.pap` sites; written as separate .class files. -/
  syntheticClasses : Array ClassFile := #[]
  nextClosureId : Nat := 0

abbrev EmitJVMM := ReaderT JVMContext (StateRefT JVMState CompilerM)

def emitOp (op : Opcode) : EmitJVMM Unit := do
  modify fun s => { s with code := op.emit s.code }

def getSlot (fvarId : FVarId) : EmitJVMM UInt8 := do
  let ctx ← read
  match ctx.varSlotMap[fvarId]? with
  | some slot => return slot
  | none => return 0

def allocSlot (fvarId : FVarId) (type : Expr) : EmitJVMM (UInt8 × JVMContext) := do
  let ctx ← read
  let slot := ctx.nextSlot
  let sz := getSlotSize type
  let newMap := ctx.varSlotMap.insert fvarId slot
  let newTypeMap := ctx.varTypeMap.insert fvarId type
  let newCtx := { ctx with varSlotMap := newMap, varTypeMap := newTypeMap, nextSlot := slot + sz }
  return (slot, newCtx)

def emitLoad (fvarId : FVarId) : EmitJVMM Unit := do
  let ctx ← read
  let slot := ctx.varSlotMap[fvarId]?.getD 0
  let ty := ctx.varTypeMap[fvarId]?.getD ImpureType.object
  match toJVMTypeDesc ty with
  | "I" => emitOp (.iload slot)
  | "J" => emitOp (.lload slot)
  | "F" => emitOp (.fload slot)
  | "D" => emitOp (.dload slot)
  | _   => emitOp (.aload slot)

def emitStoreTyped (slot : UInt8) (type : Expr) : EmitJVMM Unit := do
  match toJVMTypeDesc type with
  | "I" => emitOp (.istore slot)
  | "J" => emitOp (.lstore slot)
  | "F" => emitOp (.fstore slot)
  | "D" => emitOp (.dstore slot)
  | _   => emitOp (.astore slot)

def emitStore (fvarId : FVarId) : EmitJVMM Unit := do
  let ctx ← read
  let slot := ctx.varSlotMap[fvarId]?.getD 0
  let ty := ctx.varTypeMap[fvarId]?.getD ImpureType.object
  emitStoreTyped slot ty

def addMethodRef (className : String) (methodName : String) (desc : String) : EmitJVMM UInt16 := do
  let s ← get
  let (idx, cf') := s.cf.addMethodRef className methodName desc
  set { s with cf := cf' }
  return idx

def addClass (className : String) : EmitJVMM UInt16 := do
  let s ← get
  let (idx, cf') := s.cf.addClass className
  set { s with cf := cf' }
  return idx

def captureCode (act : EmitJVMM Unit) : EmitJVMM ByteArray := do
  let savedCode := (← get).code
  modify fun s => { s with code := ByteArray.empty }
  act
  let captured := (← get).code
  modify fun s => { s with code := savedCode }
  return captured

/--
Builds a synthetic closure classfile for a `.pap` site.

Given:
- `synClassName`  — JVM binary name of the new class, e.g. `lean/mod_Foo$Clo_3`
- `targetClass`   — JVM binary name of the class holding the target static method
- `targetMethod`  — JVM method name of the target static method
- `totalArity`    — total number of parameters the target method accepts
- `numCaptured`   — number of arguments already captured (= `args.size` at the `.pap` site)

The generated class stores captured args in `this.captured` (passed to the super constructor),
so `LeanClosure.apply` correctly merges them when building the `fullArgs` array. `invokeBody`
receives the fully-merged `fullArgs` array and calls `invokestatic` directly on it.
`copyCurried` delegates to `LeanDynamicClosure` for further partial application.
-/
def buildSyntheticClosureClass
    (synClassName targetClass targetMethod : String)
    (totalArity numCaptured : Nat) : ClassFile :=
  Id.run do
    let mut cf : ClassFile := {
      className  := synClassName
      superClass := "lean/runtime/LeanClosure"
    }
    let (leanObjClassIdx, cp0) := cf.cp.addClass "lean/runtime/LeanObject"
    cf := { cf with cp := cp0 }

    -- ========== <init>(cap0..capN-1) ==========
    -- Builds a LeanObject?[] from the constructor parameters and passes it to super, so
    -- this.captured holds all captured args. LeanClosure.apply then merges them correctly.
    let mut initParamDesc := ""
    for _ in List.range numCaptured do
      initParamDesc := initParamDesc ++ leanObjTypeDesc
    let initDesc := s!"({initParamDesc})V"
    let mut initCode := ByteArray.empty
    let (superInitIdx, cp0) := cf.cp.addMethodRef
      "lean/runtime/LeanClosure" "<init>" "(I[Llean/runtime/LeanObject;)V"
    cf := { cf with cp := cp0 }
    -- aload this
    initCode := (Opcode.aload 0).emit initCode
    -- push totalArity (LeanClosure.arity = total arity of underlying function)
    if totalArity <= 5 then
      initCode := match totalArity with
        | 0 => Opcode.iconst_0.emit initCode
        | 1 => Opcode.iconst_1.emit initCode
        | 2 => Opcode.iconst_2.emit initCode
        | 3 => Opcode.iconst_3.emit initCode
        | 4 => Opcode.iconst_4.emit initCode
        | _ => Opcode.iconst_5.emit initCode
    else if totalArity <= 127 then
      initCode := (Opcode.bipush totalArity.toUInt8).emit initCode
    else
      initCode := (Opcode.sipush totalArity.toUInt16).emit initCode
    -- push captured array
    if numCaptured == 0 then
      initCode := Opcode.aconst_null.emit initCode
    else
      -- anewarray of size numCaptured
      if numCaptured <= 5 then
        initCode := match numCaptured with
          | 0 => Opcode.iconst_0.emit initCode
          | 1 => Opcode.iconst_1.emit initCode
          | 2 => Opcode.iconst_2.emit initCode
          | 3 => Opcode.iconst_3.emit initCode
          | 4 => Opcode.iconst_4.emit initCode
          | _ => Opcode.iconst_5.emit initCode
      else if numCaptured <= 127 then
        initCode := (Opcode.bipush numCaptured.toUInt8).emit initCode
      else
        initCode := (Opcode.sipush numCaptured.toUInt16).emit initCode
      initCode := (Opcode.anewarray leanObjClassIdx).emit initCode
      let mut slot : UInt8 := 1
      for i in List.range numCaptured do
        initCode := Opcode.dup.emit initCode
        if i <= 5 then
          initCode := match i with
            | 0 => Opcode.iconst_0.emit initCode
            | 1 => Opcode.iconst_1.emit initCode
            | 2 => Opcode.iconst_2.emit initCode
            | 3 => Opcode.iconst_3.emit initCode
            | 4 => Opcode.iconst_4.emit initCode
            | _ => Opcode.iconst_5.emit initCode
        else if i <= 127 then
          initCode := (Opcode.bipush i.toUInt8).emit initCode
        else
          initCode := (Opcode.sipush i.toUInt16).emit initCode
        initCode := (Opcode.aload slot).emit initCode
        initCode := Opcode.aastore.emit initCode
        slot := slot + 1
    initCode := (Opcode.invokespecial superInitIdx).emit initCode
    initCode := Opcode.return_void.emit initCode
    let initMethod : MethodDef := {
      accessFlags := ClassFile.ACC_PUBLIC
      name := "<init>"
      descriptor := initDesc
      maxStack := 6
      maxLocals := (numCaptured + 1).toUInt16
      bytecodes := initCode
    }
    cf := { cf with methods := cf.methods.push initMethod }

    -- ========== invokeBody(args) ==========
    -- `args` is the fully-merged array built by LeanClosure.apply:
    --   args[0..numCaptured-1] = captured values
    --   args[numCaptured..totalArity-1] = newly supplied arguments
    -- We simply push args[0..totalArity-1] in order and call invokestatic.
    let invokeBodyDesc := s!"([{leanObjTypeDesc}){leanObjTypeDesc}"
    let mut bodyCode := ByteArray.empty
    for i in List.range totalArity do
      bodyCode := (Opcode.aload 1).emit bodyCode    -- load args array (slot 1)
      if i <= 5 then
        bodyCode := match i with
          | 0 => Opcode.iconst_0.emit bodyCode
          | 1 => Opcode.iconst_1.emit bodyCode
          | 2 => Opcode.iconst_2.emit bodyCode
          | 3 => Opcode.iconst_3.emit bodyCode
          | 4 => Opcode.iconst_4.emit bodyCode
          | _ => Opcode.iconst_5.emit bodyCode
      else if i <= 127 then
        bodyCode := (Opcode.bipush i.toUInt8).emit bodyCode
      else
        bodyCode := (Opcode.sipush i.toUInt16).emit bodyCode
      bodyCode := Opcode.aaload.emit bodyCode
    -- invokestatic targetClass.targetMethod(LeanObject...LeanObject)LeanObject
    let mut targetParamDesc := ""
    for _ in List.range totalArity do
      targetParamDesc := targetParamDesc ++ leanObjTypeDesc
    let targetDesc := s!"({targetParamDesc}){leanObjTypeDesc}"
    let (targetRef, cp') := cf.cp.addMethodRef targetClass targetMethod targetDesc
    cf := { cf with cp := cp' }
    bodyCode := (Opcode.invokestatic targetRef).emit bodyCode
    bodyCode := Opcode.areturn.emit bodyCode
    let invokeBodyMethod : MethodDef := {
      accessFlags := ClassFile.ACC_PUBLIC
      name := "invokeBody"
      descriptor := invokeBodyDesc
      maxStack := (totalArity + 2).toUInt16
      maxLocals := 2
      bytecodes := bodyCode
    }
    cf := { cf with methods := cf.methods.push invokeBodyMethod }

    -- ========== copyCurried(newCaptured) ==========
    -- Returns a LeanDynamicClosure carrying the merged captured array.
    -- Since this.captured holds the original caps, LeanClosure.apply passes
    -- copyCurried([cap0..capN-1, newArg0..]) i.e. the fully merged array.
    let copyCurriedDesc := s!"([{leanObjTypeDesc}){leanClosureTypeDesc}"
    let (tcStrIdx, cp2) := cf.cp.addString targetClass
    cf := { cf with cp := cp2 }
    let (tmStrIdx, cp2) := cf.cp.addString targetMethod
    cf := { cf with cp := cp2 }
    let (dynInitRef, cp2) := cf.cp.addMethodRef
      "lean/runtime/LeanDynamicClosure" "<init>"
      "(Ljava/lang/String;Ljava/lang/String;I[Llean/runtime/LeanObject;)V"
    cf := { cf with cp := cp2 }
    let (dynClassIdx, cp2) := cf.cp.addClass "lean/runtime/LeanDynamicClosure"
    cf := { cf with cp := cp2 }
    let mut copyCode := ByteArray.empty
    copyCode := (Opcode.new dynClassIdx).emit copyCode
    copyCode := Opcode.dup.emit copyCode
    if tcStrIdx <= 255 then
      copyCode := (Opcode.ldc tcStrIdx.toUInt8).emit copyCode
    else
      copyCode := (Opcode.ldc_w tcStrIdx).emit copyCode
    if tmStrIdx <= 255 then
      copyCode := (Opcode.ldc tmStrIdx.toUInt8).emit copyCode
    else
      copyCode := (Opcode.ldc_w tmStrIdx).emit copyCode
    if totalArity <= 5 then
      copyCode := match totalArity with
        | 0 => Opcode.iconst_0.emit copyCode
        | 1 => Opcode.iconst_1.emit copyCode
        | 2 => Opcode.iconst_2.emit copyCode
        | 3 => Opcode.iconst_3.emit copyCode
        | 4 => Opcode.iconst_4.emit copyCode
        | _ => Opcode.iconst_5.emit copyCode
    else if totalArity <= 127 then
      copyCode := (Opcode.bipush totalArity.toUInt8).emit copyCode
    else
      copyCode := (Opcode.sipush totalArity.toUInt16).emit copyCode
    -- newCaptured is the merged array from LeanClosure.apply; pass it directly
    copyCode := (Opcode.aload 1).emit copyCode
    copyCode := (Opcode.invokespecial dynInitRef).emit copyCode
    copyCode := Opcode.areturn.emit copyCode
    let copyCurriedMethod : MethodDef := {
      accessFlags := ClassFile.ACC_PUBLIC
      name := "copyCurried"
      descriptor := copyCurriedDesc
      maxStack := 6
      maxLocals := 2
      bytecodes := copyCode
    }
    cf := { cf with methods := cf.methods.push copyCurriedMethod }

    return cf

def addInteger (v : UInt32) : EmitJVMM UInt16 := do
  let s ← get
  let (idx, cf') := s.cf.addInteger v
  set { s with cf := cf' }
  return idx

def addLong (v : UInt64) : EmitJVMM UInt16 := do
  let s ← get
  let (idx, cf') := s.cf.addLong v
  set { s with cf := cf' }
  return idx

def emitPushLong (v : UInt64) : EmitJVMM Unit := do
  if v == 0 then
    emitOp .lconst_0
  else if v == 1 then
    emitOp .lconst_1
  else
    let idx ← addLong v
    emitOp (.ldc2_w idx)

def emitPushInt (v : Nat) : EmitJVMM Unit := do
  if v <= 5 then
    match v with
    | 0 => emitOp .iconst_0
    | 1 => emitOp .iconst_1
    | 2 => emitOp .iconst_2
    | 3 => emitOp .iconst_3
    | 4 => emitOp .iconst_4
    | _ => emitOp .iconst_5
  else if v <= 127 then
    emitOp (.bipush v.toUInt8)
  else if v <= 32767 then
    emitOp (.sipush v.toUInt16)
  else
    let idx ← addInteger v.toUInt32
    if idx <= 255 then
      emitOp (.ldc idx.toUInt8)
    else
      emitOp (.ldc_w idx)

def addString (str : String) : EmitJVMM UInt16 := do
  let s ← get
  let (idx, cf') := s.cf.addString str
  set { s with cf := cf' }
  return idx

def addFieldRef (className : String) (fieldName : String) (desc : String) : EmitJVMM UInt16 := do
  let s ← get
  let (idx, cf') := s.cf.addFieldRef className fieldName desc
  set { s with cf := cf' }
  return idx

def emitBoxValue (type : Expr) : EmitJVMM Unit := do
  match toJVMTypeDesc type with
  | "J" =>
    let ofLongIdx ← addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
    emitOp (.invokestatic ofLongIdx)
  | "I" =>
    match type with
    | ImpureType.uint8 =>
      emitPushInt 255
      emitOp .iand
      emitOp .i2l
    | ImpureType.uint16 =>
      emitPushInt 65535
      emitOp .iand
      emitOp .i2l
    | ImpureType.uint32 =>
      let toULongIdx ← addMethodRef "java/lang/Integer" "toUnsignedLong" "(I)J"
      emitOp (.invokestatic toULongIdx)
    | _ =>
      emitOp .i2l
    let ofLongIdx ← addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
    emitOp (.invokestatic ofLongIdx)
  | "D" =>
    let ofDoubleIdx ← addMethodRef "lean/runtime/LeanFloat" "ofDouble" "(D)Llean/runtime/LeanFloat;"
    emitOp (.invokestatic ofDoubleIdx)
  | _ => pure ()

def emitUnboxValue (targetType : Expr) : EmitJVMM Unit := do
  match toJVMTypeDesc targetType with
  | "J" =>
    let getScalar64Idx ← addMethodRef "lean/runtime/LeanRuntimeJVM" "getScalar64" "(Llean/runtime/LeanObject;)J"
    emitOp (.invokestatic getScalar64Idx)
  | "I" =>
    let getScalar64Idx ← addMethodRef "lean/runtime/LeanRuntimeJVM" "getScalar64" "(Llean/runtime/LeanObject;)J"
    emitOp (.invokestatic getScalar64Idx)
    emitOp .l2i
  | "D" =>
    let toDoubleIdx ← addMethodRef "lean/runtime/LeanFloat" "toDouble" "(Llean/runtime/LeanObject;)D"
    emitOp (.invokestatic toDoubleIdx)
  | _ => pure ()

/--
Emits a load of `fvarId`, converting it to `expectedType` if needed.
If `expectedType` is LeanObject and `fvarId` is scalar, boxes it.
If `expectedType` is scalar and `fvarId` is LeanObject, unboxes it.
-/
def emitLoadAs (fvarId : FVarId) (expectedType : Expr) : EmitJVMM Unit := do
  let ctx ← read
  let actualType := ctx.varTypeMap[fvarId]?.getD ImpureType.object
  let actualDesc := toJVMTypeDesc actualType
  let expectedDesc := toJVMTypeDesc expectedType
  emitLoad fvarId
  if expectedDesc == leanObjTypeDesc && actualDesc != leanObjTypeDesc then
    emitBoxValue actualType
  else if expectedDesc != leanObjTypeDesc && actualDesc == leanObjTypeDesc then
    emitUnboxValue expectedType

/--
Allocates a fresh synthetic closure class for a `.pap` site, registers it in state,
and emits `new <SynClass>; dup; [push captured args]; invokespecial <init>` into the
current code buffer.  Returns the binary name of the synthetic class.
-/
def emitSyntheticClosure
    (targetClass targetMethod : String) (totalArity : Nat)
    (args : Array (Arg .impure)) : EmitJVMM String := do
  let s ← get
  let ctx ← read
  let n := s.nextClosureId
  let synClassName := s!"{ctx.className}$$Clo_{n}"
  let numCaptured := args.size
  let synCF := buildSyntheticClosureClass synClassName targetClass targetMethod totalArity numCaptured
  modify fun st => { st with
    syntheticClasses := st.syntheticClasses.push synCF
    nextClosureId := n + 1
  }
  -- Emit allocation in the current code buffer
  let synClassIdx ← addClass synClassName
  emitOp (.new synClassIdx)
  emitOp .dup
  -- Push each captured arg
  for arg in args do
    match arg with
    | .fvar fvarId => emitLoadAs fvarId ImpureType.object
    | .erased => emitOp .aconst_null
  -- Call <init>(cap0..capN)
  let mut initParamDesc := ""
  for _ in List.range numCaptured do
    initParamDesc := initParamDesc ++ leanObjTypeDesc
  let initDesc := s!"({initParamDesc})V"
  let initRef ← addMethodRef synClassName "<init>" initDesc
  emitOp (.invokespecial initRef)
  return synClassName

def getDeclClassName (fn : Name) : EmitJVMM String := do
  let ctx ← read
  if ctx.localDecls.any (·.name == fn) then
    return ctx.className
  let env ← getEnv
  match env.getModuleIdxFor? fn with
  | some modIdx =>
    let modName := env.header.moduleNames[modIdx]!
    return toJVMClassName modName
  | none =>
    return ctx.className

def emitCtor (info : CtorInfo) (args : Array (Arg .impure)) : EmitJVMM Unit := do
  emitPushInt info.cidx
  emitPushInt info.size
  let numScalars := info.usize + (info.ssize + 7) / 8
  emitPushInt numScalars
  let allocIdx ← addMethodRef "lean/runtime/LeanCtor" "alloc" "(III)Llean/runtime/LeanCtor;"
  emitOp (.invokestatic allocIdx)
  if args.size > 0 then
    let setObjIdx ← addMethodRef "lean/runtime/LeanCtor" "setObj" "(ILlean/runtime/LeanObject;)V"
    for h : i in 0...args.size do
      let arg := args[i]
      emitOp .dup
      emitPushInt i
      match arg with
      | .fvar fvarId => emitLoadAs fvarId ImpureType.object
      | .erased => emitOp .aconst_null
      emitOp (.invokevirtual setObjIdx)

def emitPushNatLiteral (n : Nat) : EmitJVMM Unit := do
  if n <= 127 then
    emitOp (.bipush n.toUInt8)
    emitOp .i2l
    let ofLongIdx ← addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
    emitOp (.invokestatic ofLongIdx)
  else if n <= 32767 then
    emitOp (.sipush n.toUInt16)
    emitOp .i2l
    let ofLongIdx ← addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
    emitOp (.invokestatic ofLongIdx)
  else
    let strIdx ← addString (toString n)
    if strIdx <= 255 then
      emitOp (.ldc strIdx.toUInt8)
    else
      emitOp (.ldc_w strIdx)
    let ofDecStringIdx ← addMethodRef "lean/runtime/LeanNat" "ofDecString" "(Ljava/lang/String;)Llean/runtime/LeanNat;"
    emitOp (.invokestatic ofDecStringIdx)

def emitDefaultReturn (type : Expr) : EmitJVMM Unit := do
  match toJVMTypeDesc type with
  | "I" =>
    emitOp .iconst_0
    emitOp .ireturn
  | "J" =>
    emitOp .lconst_0
    emitOp .lreturn
  | "D" =>
    let toDoubleIdx ← addMethodRef "lean/runtime/LeanFloat" "toDouble" "(Llean/runtime/LeanObject;)D"
    emitOp .aconst_null
    emitOp (.invokestatic toDoubleIdx)
    emitOp .dreturn
  | _ =>
    emitOp .aconst_null
    emitOp .areturn

def emitArg (arg : Arg .impure) : EmitJVMM Unit := do
  match arg with
  | .fvar fvarId => emitLoad fvarId
  | .erased => emitOp .aconst_null

def isLongArg (ctx : JVMContext) (arg : Arg .impure) : Bool :=
  match arg with
  | .fvar fvarId =>
    let ty := ctx.varTypeMap[fvarId]?.getD ImpureType.object
    toJVMTypeDesc ty == "J"
  | _ => false

def isIntArg (ctx : JVMContext) (arg : Arg .impure) : Bool :=
  match arg with
  | .fvar fvarId =>
    let ty := ctx.varTypeMap[fvarId]?.getD ImpureType.object
    toJVMTypeDesc ty == "I"
  | _ => false

def emitBinaryArgs (args : Array (Arg .impure)) : EmitJVMM Unit := do
  emitArg args[0]!
  emitArg args[1]!

def emitPrimitiveOp? (fn : Name) (args : Array (Arg .impure)) : EmitJVMM Bool := do
  if args.size != 2 then return false
  let ctx ← read
  match fn with
  | ``UInt64.add | ``USize.add =>
    if !isLongArg ctx args[0]! || !isLongArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .ladd
    return true
  | ``UInt64.sub | ``USize.sub =>
    if !isLongArg ctx args[0]! || !isLongArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .lsub
    return true
  | ``UInt64.mul | ``USize.mul =>
    if !isLongArg ctx args[0]! || !isLongArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .lmul
    return true
  | ``UInt64.land | ``USize.land =>
    if !isLongArg ctx args[0]! || !isLongArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .land
    return true
  | ``UInt64.lor | ``USize.lor =>
    if !isLongArg ctx args[0]! || !isLongArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .lor
    return true
  | `UInt64.lxor | ``UInt64.xor | ``USize.xor | `USize.lxor =>
    if !isLongArg ctx args[0]! || !isLongArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .lxor
    return true
  | ``UInt64.shiftLeft | ``USize.shiftLeft =>
    if !isLongArg ctx args[0]! || !isLongArg ctx args[1]! then return false
    emitArg args[0]!
    emitArg args[1]!
    emitOp .l2i
    emitOp .lshl
    return true
  | ``UInt64.shiftRight | ``USize.shiftRight =>
    if !isLongArg ctx args[0]! || !isLongArg ctx args[1]! then return false
    emitArg args[0]!
    emitArg args[1]!
    emitOp .l2i
    emitOp .lushr
    return true
  | ``UInt64.div | ``USize.div =>
    if !isLongArg ctx args[0]! || !isLongArg ctx args[1]! then return false
    emitBinaryArgs args
    let ref ← addMethodRef "java/lang/Long" "divideUnsigned" "(JJ)J"
    emitOp (.invokestatic ref)
    return true
  | ``UInt64.mod | ``USize.mod =>
    if !isLongArg ctx args[0]! || !isLongArg ctx args[1]! then return false
    emitBinaryArgs args
    let ref ← addMethodRef "java/lang/Long" "remainderUnsigned" "(JJ)J"
    emitOp (.invokestatic ref)
    return true
  | ``UInt32.add =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .iadd
    return true
  | ``UInt32.sub =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .isub
    return true
  | ``UInt32.mul =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .imul
    return true
  | ``UInt32.land =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .iand
    return true
  | ``UInt32.lor =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .ior
    return true
  | `UInt32.lxor | ``UInt32.xor =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .ixor
    return true
  | ``UInt32.shiftLeft =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .ishl
    return true
  | ``UInt32.shiftRight =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .iushr
    return true
  | ``UInt32.div =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    let ref ← addMethodRef "java/lang/Integer" "divideUnsigned" "(II)I"
    emitOp (.invokestatic ref)
    return true
  | ``UInt32.mod =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    let ref ← addMethodRef "java/lang/Integer" "remainderUnsigned" "(II)I"
    emitOp (.invokestatic ref)
    return true
  | ``UInt16.add | ``UInt8.add =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .iadd
    return true
  | ``UInt16.sub | ``UInt8.sub =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .isub
    return true
  | ``UInt16.mul | ``UInt8.mul =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .imul
    return true
  | _ => return false

/--
Emits let declarations to JVM bytecode.
-/
def emitLetValue (decl : LetDecl .impure) : EmitJVMM Unit := do
  match decl.value with
  | .lit v =>
    match v with
    | .nat n => emitPushNatLiteral n
    | .str s =>
      let strIdx ← addString s
      if strIdx <= 255 then
        emitOp (.ldc strIdx.toUInt8)
      else
        emitOp (.ldc_w strIdx)
      let ofStrIdx ← addMethodRef "lean/runtime/LeanString" "of" "(Ljava/lang/String;)Llean/runtime/LeanString;"
      emitOp (.invokestatic ofStrIdx)
    | .uint8 b =>
      if isScalarType decl.type then
        emitPushInt b.toNat
      else
        emitPushNatLiteral b.toNat
    | .uint16 s =>
      if isScalarType decl.type then
        emitPushInt s.toNat
      else
        emitPushNatLiteral s.toNat
    | .uint32 i =>
      if isScalarType decl.type then
        emitPushInt i.toNat
      else
        emitPushNatLiteral i.toNat
    | .uint64 l =>
      if isScalarType decl.type then
        emitPushLong l
      else
        emitPushNatLiteral l.toNat
    | .usize u =>
      if isScalarType decl.type then
        emitPushLong u
      else
        emitPushNatLiteral u.toNat
  | .erased =>
    if toJVMTypeDesc decl.type == "I" then
      emitOp .iconst_0
    else if toJVMTypeDesc decl.type == "J" then
      emitOp .lconst_0
    else
      emitOp .aconst_null
  | .fvar fvarId args =>
    if args.isEmpty then
      emitLoad fvarId
    else
      emitLoad fvarId
      let closureClassIdx ← addClass "lean/runtime/LeanClosure"
      emitOp (.checkcast closureClassIdx)
      if args.size == 1 then
        match args[0]! with
        | .fvar argId => emitLoadAs argId ImpureType.object
        | .erased => emitOp .aconst_null
        let applyIdx ← addMethodRef "lean/runtime/LeanClosure" "apply1" "(Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
        emitOp (.invokevirtual applyIdx)
      else if args.size == 2 then
        match args[0]! with
        | .fvar argId => emitLoadAs argId ImpureType.object
        | .erased => emitOp .aconst_null
        match args[1]! with
        | .fvar argId => emitLoadAs argId ImpureType.object
        | .erased => emitOp .aconst_null
        let applyIdx ← addMethodRef "lean/runtime/LeanClosure" "apply2" "(Llean/runtime/LeanObject;Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
        emitOp (.invokevirtual applyIdx)
      else if args.size == 3 then
        match args[0]! with
        | .fvar argId => emitLoadAs argId ImpureType.object
        | .erased => emitOp .aconst_null
        match args[1]! with
        | .fvar argId => emitLoadAs argId ImpureType.object
        | .erased => emitOp .aconst_null
        match args[2]! with
        | .fvar argId => emitLoadAs argId ImpureType.object
        | .erased => emitOp .aconst_null
        let applyIdx ← addMethodRef "lean/runtime/LeanClosure" "apply3" "(Llean/runtime/LeanObject;Llean/runtime/LeanObject;Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
        emitOp (.invokevirtual applyIdx)
      else
        emitPushInt args.size
        let leanObjClassIdx ← addClass "lean/runtime/LeanObject"
        emitOp (.anewarray leanObjClassIdx)
        for h : i in 0...args.size do
          emitOp .dup
          emitPushInt i
          match args[i] with
          | .fvar argId => emitLoadAs argId ImpureType.object
          | .erased => emitOp .aconst_null
          emitOp .aastore
        let applyIdx ← addMethodRef "lean/runtime/LeanClosure" "apply" "([Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
        emitOp (.invokevirtual applyIdx)
  | .fap fn args =>
    if (← emitPrimitiveOp? fn args) then
      pure ()
    else
      let targetClass ← getDeclClassName fn
      let methodName := toJVMMethodName fn
      if targetClass.startsWith "lean/mod_l_Init_" then
        let mut pDescs := ""
        for _ in args do
          pDescs := pDescs ++ leanObjTypeDesc
        let descriptor := s!"({pDescs}){leanObjTypeDesc}"
        for arg in args do
          match arg with
          | .fvar fvarId => emitLoadAs fvarId ImpureType.object
          | .erased => emitOp .aconst_null
        let methodRef ← addMethodRef targetClass methodName descriptor
        emitOp (.invokestatic methodRef)
        if toJVMTypeDesc decl.type != leanObjTypeDesc then
          emitUnboxValue decl.type
      else
        let sig? ← getImpureSignature? fn
        let localDecl? := (← read).localDecls.find? (·.name == fn)
        let (paramTypes, retDesc) := match localDecl? with
          | some decl => (decl.params.map fun (p : Param .impure) => p.type, toJVMTypeDesc decl.type)
          | none => match sig? with
            | some sig => (sig.params.map fun (p : Param .impure) => p.type, toJVMTypeDesc sig.type)
            | none => (args.map fun _ => ImpureType.object, leanObjTypeDesc)
        let mut pDescs := ""
        for p in paramTypes do
          pDescs := pDescs ++ toJVMTypeDesc p
        let descriptor := s!"({pDescs}){retDesc}"
        for h : i in 0...args.size do
          let arg := args[i]
          let pTy := if h2 : i < paramTypes.size then paramTypes[i] else ImpureType.object
          match arg with
          | .fvar fvarId => emitLoadAs fvarId pTy
          | .erased =>
            if toJVMTypeDesc pTy == "I" then emitOp .iconst_0
            else if toJVMTypeDesc pTy == "J" then emitOp .lconst_0
            else emitOp .aconst_null
        let methodRef ← addMethodRef targetClass methodName descriptor
        emitOp (.invokestatic methodRef)
  | .pap fn args =>
    let targetClass ← getDeclClassName fn
    let methodName := toJVMMethodName fn
    let sig? ← getImpureSignature? fn
    let totalArity? := match (← read).localDecls.find? (·.name == fn) with
      | some decl => some decl.params.size
      | none => sig?.map (·.params.size)
    -- Fall back to dynamic dispatch (LeanClosure.alloc) when we cannot determine the
    -- total arity statically or when all args are already captured (remainingArity = 0).
    match totalArity? with
    | some totalArity =>
      if totalArity > args.size then
        discard <| emitSyntheticClosure targetClass methodName totalArity args
      else
        -- All args already captured (remainingArity = 0); dynamic fallback.
        let classStrRef ← addString targetClass
        if classStrRef <= 255 then emitOp (.ldc classStrRef.toUInt8)
        else emitOp (.ldc_w classStrRef)
        let methodStrRef ← addString methodName
        if methodStrRef <= 255 then emitOp (.ldc methodStrRef.toUInt8)
        else emitOp (.ldc_w methodStrRef)
        emitPushInt totalArity
        emitPushInt args.size
        let leanObjClassIdx ← addClass "lean/runtime/LeanObject"
        emitOp (.anewarray leanObjClassIdx)
        for h : i in 0...args.size do
          emitOp .dup
          emitPushInt i
          let arg := args[i]
          match arg with
          | .fvar argId => emitLoadAs argId ImpureType.object
          | .erased => emitOp .aconst_null
          emitOp .aastore
        let allocRef ← addMethodRef "lean/runtime/LeanClosure" "alloc"
          "(Ljava/lang/String;Ljava/lang/String;I[Llean/runtime/LeanObject;)Llean/runtime/LeanClosure;"
        emitOp (.invokestatic allocRef)
    | none =>
      -- Arity unknown; dynamic fallback using captured count as best estimate.
      let classStrRef ← addString targetClass
      if classStrRef <= 255 then emitOp (.ldc classStrRef.toUInt8)
      else emitOp (.ldc_w classStrRef)
      let methodStrRef ← addString methodName
      if methodStrRef <= 255 then emitOp (.ldc methodStrRef.toUInt8)
      else emitOp (.ldc_w methodStrRef)
      emitPushInt args.size
      emitPushInt args.size
      let leanObjClassIdx ← addClass "lean/runtime/LeanObject"
      emitOp (.anewarray leanObjClassIdx)
      for h : i in 0...args.size do
        emitOp .dup
        emitPushInt i
        let arg := args[i]
        match arg with
        | .fvar argId => emitLoadAs argId ImpureType.object
        | .erased => emitOp .aconst_null
        emitOp .aastore
      let allocRef ← addMethodRef "lean/runtime/LeanClosure" "alloc"
        "(Ljava/lang/String;Ljava/lang/String;I[Llean/runtime/LeanObject;)Llean/runtime/LeanClosure;"
      emitOp (.invokestatic allocRef)
  | .ctor info args =>
    emitCtor info args
  | .oproj i fvarId =>
    emitLoad fvarId
    let ctorClassIdx ← addClass "lean/runtime/LeanCtor"
    emitOp (.checkcast ctorClassIdx)
    emitPushInt i
    let getObjIdx ← addMethodRef "lean/runtime/LeanCtor" "getObj" "(I)Llean/runtime/LeanObject;"
    emitOp (.invokevirtual getObjIdx)
  | .uproj i fvarId =>
    emitLoad fvarId
    let ctorClassIdx ← addClass "lean/runtime/LeanCtor"
    emitOp (.checkcast ctorClassIdx)
    emitPushInt i
    let getScalarIdx ← addMethodRef "lean/runtime/LeanCtor" "getScalar" "(I)J"
    emitOp (.invokevirtual getScalarIdx)
    if isScalarType decl.type then
      if toJVMTypeDesc decl.type == "I" then
        emitOp .l2i
    else
      let ofLongIdx ← addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
      emitOp (.invokestatic ofLongIdx)
  | .sproj n offset fvarId =>
    emitLoad fvarId
    let ctorClassIdx ← addClass "lean/runtime/LeanCtor"
    emitOp (.checkcast ctorClassIdx)
    emitPushInt n
    emitPushInt offset
    let numBytes := getScalarNumBytes decl.type
    emitPushInt numBytes
    let getScalarIdx ← addMethodRef "lean/runtime/LeanCtor" "getByteScalar" "(III)J"
    emitOp (.invokevirtual getScalarIdx)
    if isScalarType decl.type then
      if decl.type == ImpureType.uint8 || decl.type == ImpureType.uint16 || decl.type == ImpureType.uint32 then
        emitOp .l2i
      else if decl.type == ImpureType.float then
        let toDoubleIdx ← addMethodRef "java/lang/Double" "longBitsToDouble" "(J)D"
        emitOp (.invokestatic toDoubleIdx)
    else
      let ofLongIdx ← addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
      emitOp (.invokestatic ofLongIdx)
  | .box ty fvarId =>
    let ctx ← read
    let actualType := ctx.varTypeMap[fvarId]?.getD ty
    emitLoad fvarId
    if toJVMTypeDesc actualType != leanObjTypeDesc then
      emitBoxValue actualType
  | .unbox fvarId =>
    emitLoadAs fvarId decl.type
  | .reuse _ info _ args =>
    emitCtor info args
  | .reset .. =>
    emitOp .aconst_null
  | .isShared _fvarId =>
    emitOp .iconst_1
    if !isScalarType decl.type then
      emitOp .i2l
      let ofLongIdx ← addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
      emitOp (.invokestatic ofLongIdx)
  | _ =>
    if toJVMTypeDesc decl.type == "I" then
      emitOp .iconst_0
    else if toJVMTypeDesc decl.type == "J" then
      emitOp .lconst_0
    else
      emitOp .aconst_null

/--
Emits a basic block of LCNF code to JVM bytecode.
-/
partial def emitCode (code : Code .impure) : EmitJVMM Unit := do
  match code with
  | .let decl k =>
    emitLetValue decl
    let actualType := match decl.value with
      | .ctor .. | .box .. | .lit (.nat ..) => ImpureType.object
      | _ => decl.type
    let (slot, newCtx) ← allocSlot decl.fvarId actualType
    emitStoreTyped slot actualType
    withReader (fun _ => newCtx) do
      emitCode k
  | .return fvarId =>
    let retType := (← read).currReturnType
    emitLoadAs fvarId retType
    match toJVMTypeDesc retType with
    | "I" => emitOp .ireturn
    | "J" => emitOp .lreturn
    | "F" => emitOp .freturn
    | "D" => emitOp .dreturn
    | _   => emitOp .areturn
  | .unreach .. =>
    let retType := (← read).currReturnType
    emitDefaultReturn retType
  | .cases cs =>
    if cs.alts.isEmpty then
      let retType := (← read).currReturnType
      emitDefaultReturn retType
    else if cs.alts.size == 1 then
      match cs.alts[0]! with
      | .ctorAlt _ k => emitCode k
      | .default k => emitCode k
    else
      let ctx ← read
      let tagSlot := ctx.nextSlot
      let branchCtx := { ctx with nextSlot := ctx.nextSlot + 1 }
      let discrTy := ctx.varTypeMap[cs.discr]?.getD ImpureType.object
      if toJVMTypeDesc discrTy == leanObjTypeDesc then
        emitLoad cs.discr
        let getTagIdx ← addMethodRef "lean/runtime/LeanObject" "getTag" "()I"
        emitOp (.invokevirtual getTagIdx)
      else
        emitLoad cs.discr
        if toJVMTypeDesc discrTy == "J" then
          emitOp .l2i
      emitOp (.istore tagSlot)
      withReader (fun _ => branchCtx) do
        let hasDefault := cs.alts.any fun
          | .default .. => true
          | _ => false
        for alt in cs.alts do
          match alt with
          | .ctorAlt info k =>
            let altCode ← captureCode (emitCode k)
            emitOp (.iload tagSlot)
            emitPushInt info.cidx
            let jumpOffset := (3 + altCode.size).toUInt16
            emitOp (.if_icmpne jumpOffset)
            modify fun s => { s with code := s.code ++ altCode }
          | .default k =>
            emitCode k
        if !hasDefault then
          let retType := (← read).currReturnType
          emitDefaultReturn retType
  | .jmp fvarId args =>
    let ctx ← read
    match ctx.joinPoints[fvarId]? with
    | some decl =>
      let mut nextSlot := ctx.nextSlot
      let mut newVarSlotMap := ctx.varSlotMap
      let mut newVarTypeMap := ctx.varTypeMap
      let mut paramSlots : Array (Param .impure × UInt8) := #[]
      for p in decl.params do
        paramSlots := paramSlots.push (p, nextSlot)
        newVarSlotMap := newVarSlotMap.insert p.fvarId nextSlot
        newVarTypeMap := newVarTypeMap.insert p.fvarId p.type
        nextSlot := nextSlot + getSlotSize p.type

      for h : i in 0...args.size do
        let arg := args[i]
        if h2 : i < paramSlots.size then
          let (p, slot) := paramSlots[i]
          match arg with
          | .fvar id => emitLoadAs id p.type
          | .erased =>
            if toJVMTypeDesc p.type == "I" then
              emitOp .iconst_0
            else if toJVMTypeDesc p.type == "J" then
              emitOp .lconst_0
            else
              emitOp .aconst_null
          emitStoreTyped slot p.type
      let newCtx := { ctx with varSlotMap := newVarSlotMap, varTypeMap := newVarTypeMap, nextSlot := nextSlot }
      withReader (fun _ => newCtx) do
        emitCode decl.value
    | none =>
      let retType := ctx.currReturnType
      emitDefaultReturn retType
  | .jp decl k =>
    let newCtx := { (← read) with joinPoints := (← read).joinPoints.insert decl.fvarId decl }
    withReader (fun _ => newCtx) do
      emitCode k
  | .inc (k := k) .. | .dec (k := k) .. | .del (k := k) .. =>
    -- Tracing GC: Reference counting instructions are no-ops!
    emitCode k
  | .oset fvarId i y k =>
    emitLoad fvarId
    let ctorClassIdx ← addClass "lean/runtime/LeanCtor"
    emitOp (.checkcast ctorClassIdx)
    emitPushInt i
    match y with
    | .fvar yId => emitLoadAs yId ImpureType.object
    | .erased => emitOp .aconst_null
    let setObjIdx ← addMethodRef "lean/runtime/LeanCtor" "setObj" "(ILlean/runtime/LeanObject;)V"
    emitOp (.invokevirtual setObjIdx)
    emitCode k
  | .uset fvarId i y k =>
    emitLoad fvarId
    let ctorClassIdx ← addClass "lean/runtime/LeanCtor"
    emitOp (.checkcast ctorClassIdx)
    emitPushInt i
    let ctx ← read
    let yType := ctx.varTypeMap[y]?.getD ImpureType.object
    emitLoad y
    if toJVMTypeDesc yType == "J" then
      pure ()
    else if toJVMTypeDesc yType == "I" then
      emitOp .i2l
    else
      let getScalar64Idx ← addMethodRef "lean/runtime/LeanRuntimeJVM" "getScalar64" "(Llean/runtime/LeanObject;)J"
      emitOp (.invokestatic getScalar64Idx)
    let setScalarIdx ← addMethodRef "lean/runtime/LeanCtor" "setScalar" "(IJ)V"
    emitOp (.invokevirtual setScalarIdx)
    emitCode k
  | .sset fvarId i offset y ty k =>
    emitLoad fvarId
    let ctorClassIdx ← addClass "lean/runtime/LeanCtor"
    emitOp (.checkcast ctorClassIdx)
    emitPushInt i
    emitPushInt offset
    let numBytes := getScalarNumBytes ty
    emitPushInt numBytes
    let ctx ← read
    let yType := ctx.varTypeMap[y]?.getD ty
    emitLoad y
    if toJVMTypeDesc yType == "I" then
      emitOp .i2l
    else if toJVMTypeDesc yType == "D" then
      let toRawBitsIdx ← addMethodRef "java/lang/Double" "doubleToRawLongBits" "(D)J"
      emitOp (.invokestatic toRawBitsIdx)
    else if toJVMTypeDesc yType != "J" then
      let getScalar64Idx ← addMethodRef "lean/runtime/LeanRuntimeJVM" "getScalar64" "(Llean/runtime/LeanObject;)J"
      emitOp (.invokestatic getScalar64Idx)
    let setScalarIdx ← addMethodRef "lean/runtime/LeanCtor" "setByteScalar" "(IIIJ)V"
    emitOp (.invokevirtual setScalarIdx)
    emitCode k
  | .setTag _ _ k =>
    emitCode k

/--
Emits a function declaration as a static method in the classfile.
-/
def emitFnDecl (decl : Decl .impure) : EmitJVMM Unit := do
  match decl.value with
  | .extern .. =>
    let env ← getEnv
    if (getInitFnNameFor? env decl.name).isSome then
      let fieldName := toJVMMethodName decl.name
      let typeDesc := toJVMTypeDesc decl.type
      let fieldDef : FieldDef := {
        accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
        name := fieldName
        descriptor := typeDesc
      }
      modify fun s => { s with cf := { s.cf with fields := s.cf.fields.push fieldDef } }

      let className := (← read).className
      let fieldRef ← addFieldRef className fieldName typeDesc
      let mut getterCode := ByteArray.empty
      getterCode := (Opcode.getstatic fieldRef).emit getterCode
      match typeDesc with
      | "I" => getterCode := Opcode.ireturn.emit getterCode
      | "J" => getterCode := Opcode.lreturn.emit getterCode
      | "D" => getterCode := Opcode.dreturn.emit getterCode
      | "F" => getterCode := Opcode.freturn.emit getterCode
      | _   => getterCode := Opcode.areturn.emit getterCode
      let getterDef : MethodDef := {
        accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
        name := fieldName
        descriptor := s!"(){typeDesc}"
        maxStack := 2
        maxLocals := 0
        bytecodes := getterCode
      }
      modify fun s => { s with cf := { s.cf with methods := s.cf.methods.push getterDef } }
    return ()
  | .code code =>
    let methodName := toJVMMethodName decl.name
    let isBoxed := isBoxedName decl.name
    let mut paramDescs := ""
    let mut slot : UInt8 := 0
    let mut slotMap : Std.HashMap FVarId UInt8 := {}
    let mut typeMap : Std.HashMap FVarId Expr := {}
    for p in decl.params do
      let pTy := if isBoxed then ImpureType.object else p.type
      slotMap := slotMap.insert p.fvarId slot
      typeMap := typeMap.insert p.fvarId pTy
      paramDescs := paramDescs ++ toJVMTypeDesc pTy
      slot := slot + getSlotSize pTy

    let retType := if isBoxed then ImpureType.object else decl.type
    let retDesc := toJVMTypeDesc retType
    let descriptor := s!"({paramDescs}){retDesc}"

    -- Reset code buffer for this method
    modify fun s => { s with code := ByteArray.empty }

    let ctx ← read
    let methodCtx := { ctx with
      currFn := decl.name
      currReturnType := retType
      currParams := decl.params
      varSlotMap := slotMap
      varTypeMap := typeMap
      nextSlot := slot
    }

    withReader (fun _ => methodCtx) do
      emitCode code

    let bytecodes := (← get).code
    let methodDef : MethodDef := {
      accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
      name := methodName
      descriptor := descriptor
      maxStack := 64
      maxLocals := 255
      bytecodes := bytecodes
    }

    modify fun s => { s with cf := { s.cf with methods := s.cf.methods.push methodDef } }

/--
Emits module-level initialization code (<clinit> and public static void initialize()).
Initializes imported modules, then runs local initializers in declaration order.
-/
def emitModuleInit (decls : Array Name) : EmitJVMM Unit := do
  let env ← getEnv
  let className := (← read).className
  let indexMap := getImpureDeclIndices env decls

  -- Add static field: private static boolean _G_initialized = false
  let initFieldDef : FieldDef := {
    accessFlags := ClassFile.ACC_PRIVATE ||| ClassFile.ACC_STATIC
    name := "_G_initialized"
    descriptor := "Z"
  }
  modify fun s => { s with cf := { s.cf with fields := s.cf.fields.push initFieldDef } }

  let initFieldRef ← addFieldRef className "_G_initialized" "Z"

  -- Build initialize() method body
  let mut initBody := ByteArray.empty

  -- 1. Initialize imported modules
  for imp in env.imports do
    let impClass := toJVMClassName imp.module
    let impClassIdx ← addString impClass
    if impClassIdx <= 255 then
      initBody := (Opcode.ldc impClassIdx.toUInt8).emit initBody
    else
      initBody := (Opcode.ldc_w impClassIdx).emit initBody
    let initModRef ← addMethodRef "lean/runtime/LeanRuntimeJVM" "initializeModule" "(Ljava/lang/String;)V"
    initBody := (Opcode.invokestatic initModRef).emit initBody

  -- 2. Sort decls by declaration index to run local initializers in order
  let sortedDecls := decls.qsort fun l r =>
    indexMap.getD l 0 < indexMap.getD r 0

  for declName in sortedDecls do
    if isIOUnitInitFn env declName then
      let methodName := toJVMMethodName declName
      let methodRef ← addMethodRef className methodName s!"({leanObjTypeDesc}){leanObjTypeDesc}"
      initBody := Opcode.aconst_null.emit initBody
      initBody := (Opcode.invokestatic methodRef).emit initBody
      initBody := Opcode.pop.emit initBody
    else if let some initFn := getInitFnNameFor? env declName then
      let initMethodName := toJVMMethodName initFn
      let initMethodRef ← addMethodRef className initMethodName s!"({leanObjTypeDesc}){leanObjTypeDesc}"
      initBody := Opcode.aconst_null.emit initBody
      initBody := (Opcode.invokestatic initMethodRef).emit initBody
      let getValRef ← addMethodRef "lean/runtime/LeanRuntimeJVM" "ioResultGetValue" s!"({leanObjTypeDesc}){leanObjTypeDesc}"
      initBody := (Opcode.invokestatic getValRef).emit initBody
      let declType ← match (← getImpureSignature? declName) with
        | some sig => pure sig.type
        | none => match (← read).localDecls.find? (·.name == declName) with
          | some d => pure d.type
          | none => pure ImpureType.object
      let typeDesc := toJVMTypeDesc declType
      if typeDesc == "J" then
        let getScalar64Idx ← addMethodRef "lean/runtime/LeanRuntimeJVM" "getScalar64" "(Llean/runtime/LeanObject;)J"
        initBody := (Opcode.invokestatic getScalar64Idx).emit initBody
      else if typeDesc == "I" then
        let getScalar64Idx ← addMethodRef "lean/runtime/LeanRuntimeJVM" "getScalar64" "(Llean/runtime/LeanObject;)J"
        initBody := (Opcode.invokestatic getScalar64Idx).emit initBody
        initBody := Opcode.l2i.emit initBody
      else if typeDesc == "D" then
        let toDoubleIdx ← addMethodRef "lean/runtime/LeanFloat" "toDouble" "(Llean/runtime/LeanObject;)D"
        initBody := (Opcode.invokestatic toDoubleIdx).emit initBody
      let declFieldRef ← addFieldRef className (toJVMMethodName declName) typeDesc
      initBody := (Opcode.putstatic declFieldRef).emit initBody

  -- Method code for initialize():
  -- if (_G_initialized) return;
  -- _G_initialized = true;
  -- <initBody>
  -- return;
  let mut fullCode := ByteArray.empty
  fullCode := (Opcode.getstatic initFieldRef).emit fullCode
  let skipOffset := (7 + initBody.size).toUInt16
  fullCode := (Opcode.ifne skipOffset).emit fullCode
  fullCode := Opcode.iconst_1.emit fullCode
  fullCode := (Opcode.putstatic initFieldRef).emit fullCode
  fullCode := fullCode ++ initBody
  fullCode := Opcode.return_void.emit fullCode

  let initMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "initialize"
    descriptor := "()V"
    maxStack := 4
    maxLocals := 0
    bytecodes := fullCode
  }
  modify fun s => { s with cf := { s.cf with methods := s.cf.methods.push initMethod } }

  -- Emit <clinit>()V:
  -- invokestatic className.initialize:()V
  -- return
  let selfInitRef ← addMethodRef className "initialize" "()V"
  let mut clinitCode := ByteArray.empty
  clinitCode := (Opcode.invokestatic selfInitRef).emit clinitCode
  clinitCode := Opcode.return_void.emit clinitCode
  let clinitMethod : MethodDef := {
    accessFlags := ClassFile.ACC_STATIC
    name := "<clinit>"
    descriptor := "()V"
    maxStack := 1
    maxLocals := 0
    bytecodes := clinitCode
  }
  modify fun s => { s with cf := { s.cf with methods := s.cf.methods.push clinitMethod } }

/--
Emits a public static void main entry point if the module contains a `main` declaration.
-/
def emitMainIfNeeded : EmitJVMM Unit := do
  let mainDecl? := (← read).localDecls.find? (·.name == `main)
  if let some decl := mainDecl? then
    let arity := decl.params.size
    let className := (← read).className
    let descriptor := if arity == 2 then s!"({leanObjTypeDesc}{leanObjTypeDesc}){leanObjTypeDesc}" else s!"({leanObjTypeDesc}){leanObjTypeDesc}"
    let mainMethodRef ← addMethodRef className "f_main" descriptor
    let mut code := ByteArray.empty
    if arity == 2 then
      let convRef ← addMethodRef "lean/runtime/LeanRuntimeJVM" "stringArrayToList" s!"([Ljava/lang/String;){leanObjTypeDesc}"
      code := (Opcode.aload 0).emit code
      code := (Opcode.invokestatic convRef).emit code
      code := Opcode.aconst_null.emit code
      code := (Opcode.invokestatic mainMethodRef).emit code
    else
      code := Opcode.aconst_null.emit code
      code := (Opcode.invokestatic mainMethodRef).emit code
    let handleRef ← addMethodRef "lean/runtime/LeanRuntimeJVM" "handleIOResult" s!"({leanObjTypeDesc})V"
    code := (Opcode.invokestatic handleRef).emit code
    code := Opcode.return_void.emit code

    let mainMethod : MethodDef := {
      accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
      name := "main"
      descriptor := "([Ljava/lang/String;)V"
      maxStack := 4
      maxLocals := 1
      bytecodes := code
    }
    modify fun s => { s with cf := { s.cf with methods := s.cf.methods.push mainMethod } }

/--
Emits static fields and getter methods for initialized declarations that do not have their own function body in LCNF.
-/
def emitMissingInitGetters (decls : Array Name) : EmitJVMM Unit := do
  let env ← getEnv
  let className := (← read).className
  for declName in decls do
    if (getInitFnNameFor? env declName).isSome then
      let fieldName := toJVMMethodName declName
      if !(← get).cf.fields.any (·.name == fieldName) then
        let declType ← match (← getImpureSignature? declName) with
          | some sig => pure sig.type
          | none => match (← read).localDecls.find? (·.name == declName) with
            | some d => pure d.type
            | none => pure ImpureType.object
        let typeDesc := toJVMTypeDesc declType
        let fieldDef : FieldDef := {
          accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
          name := fieldName
          descriptor := typeDesc
        }
        modify fun (s : JVMState) => { s with cf := { s.cf with fields := s.cf.fields.push fieldDef } }
        let fieldRef ← addFieldRef className fieldName typeDesc
        let mut getterCode := ByteArray.empty
        getterCode := (Opcode.getstatic fieldRef).emit getterCode
        match typeDesc with
        | "I" => getterCode := Opcode.ireturn.emit getterCode
        | "J" => getterCode := Opcode.lreturn.emit getterCode
        | "D" => getterCode := Opcode.dreturn.emit getterCode
        | "F" => getterCode := Opcode.freturn.emit getterCode
        | _   => getterCode := Opcode.areturn.emit getterCode
        let getterDef : MethodDef := {
          accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
          name := fieldName
          descriptor := s!"(){typeDesc}"
          maxStack := 2
          maxLocals := 0
          bytecodes := getterCode
        }
        modify fun (s : JVMState) => { s with cf := { s.cf with methods := s.cf.methods.push getterDef } }

/--
Top-level entry point emitting JVM class bytecode for a set of declarations.
Returns an array of `(className, bytes)` pairs — the first element is always the
module's main class; additional elements are synthetic closure classes.
-/
public def emitJVMForDecls (modName : Name) (decls : Array Name) : CoreM (Array (String × ByteArray)) := do
  let (localDecls, otherModuleDecls) ← collectUsedDecls decls
  let env ← getEnv
  let indexMap := getImpureDeclIndices env decls
  let localDecls := localDecls.qsort fun l r => indexMap[l.name]! < indexMap[r.name]!

  let className := toJVMClassName modName
  let initialCF : ClassFile := {
    className := className
    superClass := "java/lang/Object"
  }

  let ctx : JVMContext := {
    modName := modName
    className := className
    localDecls := localDecls
    otherModuleDecls := otherModuleDecls
  }

  let (_, s) ← (do
    for decl in localDecls do
      emitFnDecl decl
    emitMissingInitGetters decls
    emitModuleInit decls
    emitMainIfNeeded
  : EmitJVMM Unit).run ctx |>.run { cf := initialCF } |>.run (phase := .impure)

  -- Build result: main class first, then synthetic closure classes
  let mut result : Array (String × ByteArray) := #[(className, s.cf.toByteArray)]
  for synCF in s.syntheticClasses do
    result := result.push (synCF.className, synCF.toByteArray)
  return result

/--
Emits JVM bytecode (.class files) for the given module.
Returns an array of `(className, bytes)` pairs.
-/
public def emitJVM (modName : Name) : CoreM (Array (String × ByteArray)) := do
  emitJVMForDecls modName (← getLocalImpureDecls)

end JVM

end

end Lean.Compiler.LCNF
