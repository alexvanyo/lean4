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
  ctorClassMap     : Std.HashMap FVarId (Name × CtorLayout) := {}
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

def captureCode (act : EmitJVMM Unit) : EmitJVMM (ByteArray × Array (Nat × Name) × Array (Name × Nat)) := do
  let savedCode := (← get).code
  let savedFixups := (← get).fixups
  let savedLabels := (← get).labelOffsets
  modify fun s => { s with code := ByteArray.empty, fixups := #[], labelOffsets := {} }
  act
  let capturedCode := (← get).code
  let capturedFixups := (← get).fixups
  let capturedLabels := (← get).labelOffsets
  modify fun s => { s with code := savedCode, fixups := savedFixups, labelOffsets := savedLabels }
  return (capturedCode, capturedFixups, capturedLabels.toArray)

def appendCode (code : ByteArray) (fixups : Array (Nat × Name) := #[]) (labels : Array (Name × Nat) := #[]) : EmitJVMM Unit := do
  let base := (← get).code.size
  modify fun s => {
    s with
    code := s.code ++ code
    fixups := s.fixups ++ fixups.map (fun (fpc, name) => (base + fpc, name))
    labelOffsets := labels.foldl (fun m (name, off) => m.insert name (base + off)) s.labelOffsets
  }

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

def addMethodHandle (refKind : UInt8) (refIdx : UInt16) : EmitJVMM UInt16 := do
  let s ← get
  let (idx, cf') := s.cf.addMethodHandle refKind refIdx
  set { s with cf := cf' }
  return idx

def addMethodType (desc : String) : EmitJVMM UInt16 := do
  let s ← get
  let (idx, cf') := s.cf.addMethodType desc
  set { s with cf := cf' }
  return idx

def addInvokeDynamic (bmAttrIdx : UInt16) (name : String) (desc : String) : EmitJVMM UInt16 := do
  let s ← get
  let (idx, cf') := s.cf.addInvokeDynamic bmAttrIdx name desc
  set { s with cf := cf' }
  return idx

def addBootstrapMethod (bmRef : UInt16) (args : Array UInt16) : EmitJVMM UInt16 := do
  let s ← get
  let (idx, cf') := s.cf.addBootstrapMethod bmRef args
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

def emitCtorArg (arg : Arg .impure) : EmitJVMM Unit := do
  match arg with
  | .fvar fvarId => emitLoadAs fvarId ImpureType.object
  | .erased => emitOp .aconst_null






def emitCtorClass (ctorName : Name) (layout : CtorLayout) : EmitJVMM String := do
  let className := "lean/ctor_" ++ ctorName.mangle
  let s ← get
  if s.syntheticClasses.any (·.className == className) then
    return className

  let mut cf : ClassFile := {
    className := className
    superClass := "lean/runtime/LeanCtor"
  }

  let csize := layout.ctorInfo.size
  let usize := layout.ctorInfo.usize

  for i in [0:csize] do
    cf := { cf with fields := cf.fields.push { accessFlags := ClassFile.ACC_PUBLIC, name := s!"obj_{i}", descriptor := "Llean/runtime/LeanObject;" } }

  for i in [0:usize] do
    cf := { cf with fields := cf.fields.push { accessFlags := ClassFile.ACC_PUBLIC, name := s!"usize_{csize + i}", descriptor := "J" } }

  let mut hasScalars := false
  for f in layout.fieldInfo do
    if let .scalar _ offset type := f then
      hasScalars := true
      cf := { cf with fields := cf.fields.push { accessFlags := ClassFile.ACC_PUBLIC, name := s!"s_{offset}", descriptor := toJVMTypeDesc type } }

  let mut initDesc := "("
  for _ in [0:csize] do
    initDesc := initDesc ++ "Llean/runtime/LeanObject;"
  initDesc := initDesc ++ ")V"

  let (initRef, cf') := cf.addMethodRef "lean/runtime/LeanCtor" "<init>" "(I)V"
  cf := cf'
  
  let mut initCode := ByteArray.empty
  initCode := (Opcode.aload 0).emit initCode
  if layout.ctorInfo.cidx <= 127 then
    initCode := (Opcode.bipush layout.ctorInfo.cidx.toUInt8).emit initCode
  else
    initCode := (Opcode.sipush layout.ctorInfo.cidx.toUInt16).emit initCode
  initCode := (Opcode.invokespecial initRef).emit initCode

  for i in [0:csize] do
    initCode := (Opcode.aload 0).emit initCode
    if i == 0 then initCode := (Opcode.aload 1).emit initCode
    else if i == 1 then initCode := (Opcode.aload 2).emit initCode
    else if i == 2 then initCode := (Opcode.aload 3).emit initCode
    else initCode := (Opcode.aload (i + 1).toUInt8).emit initCode
    let (fieldRef, cf') := cf.addFieldRef className s!"obj_{i}" "Llean/runtime/LeanObject;"
    cf := cf'
    initCode := (Opcode.putfield fieldRef).emit initCode

  initCode := Opcode.return_void.emit initCode

  let initMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC
    name := "<init>"
    descriptor := initDesc
    maxStack := 3
    maxLocals := (csize + 1).toUInt16
    bytecodes := initCode
  }
  cf := { cf with methods := cf.methods.push initMethod }

  let mut getObjCode := ByteArray.empty
  let mut setObjCode := ByteArray.empty

  for i in [0:csize] do
    let (fieldRef, cf') := cf.addFieldRef className s!"obj_{i}" "Llean/runtime/LeanObject;"
    cf := cf'
    
    getObjCode := (Opcode.iload 1).emit getObjCode
    if i <= 127 then getObjCode := (Opcode.bipush i.toUInt8).emit getObjCode
    else getObjCode := (Opcode.sipush i.toUInt16).emit getObjCode
    let mut blk := ByteArray.empty
    blk := (Opcode.aload 0).emit blk
    blk := (Opcode.getfield fieldRef).emit blk
    blk := Opcode.areturn.emit blk
    getObjCode := (Opcode.if_icmpne (3 + blk.size).toUInt16).emit getObjCode
    getObjCode := getObjCode ++ blk
    
    setObjCode := (Opcode.iload 1).emit setObjCode
    if i <= 127 then setObjCode := (Opcode.bipush i.toUInt8).emit setObjCode
    else setObjCode := (Opcode.sipush i.toUInt16).emit setObjCode
    let mut blk2 := ByteArray.empty
    blk2 := (Opcode.aload 0).emit blk2
    blk2 := (Opcode.aload 2).emit blk2
    blk2 := (Opcode.putfield fieldRef).emit blk2
    blk2 := Opcode.return_void.emit blk2
    setObjCode := (Opcode.if_icmpne (3 + blk2.size).toUInt16).emit setObjCode
    setObjCode := setObjCode ++ blk2
    
    if i == 0 then
      let mut gb0 := ByteArray.empty
      gb0 := (Opcode.aload 0).emit gb0
      gb0 := (Opcode.getfield fieldRef).emit gb0
      gb0 := Opcode.areturn.emit gb0
      let mDef : MethodDef := { accessFlags := ClassFile.ACC_PUBLIC, name := "getObj0", descriptor := "()Llean/runtime/LeanObject;", maxStack := 1, maxLocals := 1, bytecodes := gb0 }
      cf := { cf with methods := cf.methods.push mDef }
      let mut sb0 := ByteArray.empty
      sb0 := (Opcode.aload 0).emit sb0
      sb0 := (Opcode.aload 1).emit sb0
      sb0 := (Opcode.putfield fieldRef).emit sb0
      sb0 := Opcode.return_void.emit sb0
      let mDef2 : MethodDef := { accessFlags := ClassFile.ACC_PUBLIC, name := "setObj0", descriptor := "(Llean/runtime/LeanObject;)V", maxStack := 2, maxLocals := 2, bytecodes := sb0 }
      cf := { cf with methods := cf.methods.push mDef2 }
    else if i == 1 then
      let mut gb1 := ByteArray.empty
      gb1 := (Opcode.aload 0).emit gb1
      gb1 := (Opcode.getfield fieldRef).emit gb1
      gb1 := Opcode.areturn.emit gb1
      let mDef : MethodDef := { accessFlags := ClassFile.ACC_PUBLIC, name := "getObj1", descriptor := "()Llean/runtime/LeanObject;", maxStack := 1, maxLocals := 1, bytecodes := gb1 }
      cf := { cf with methods := cf.methods.push mDef }
      let mut sb1 := ByteArray.empty
      sb1 := (Opcode.aload 0).emit sb1
      sb1 := (Opcode.aload 1).emit sb1
      sb1 := (Opcode.putfield fieldRef).emit sb1
      sb1 := Opcode.return_void.emit sb1
      let mDef2 : MethodDef := { accessFlags := ClassFile.ACC_PUBLIC, name := "setObj1", descriptor := "(Llean/runtime/LeanObject;)V", maxStack := 2, maxLocals := 2, bytecodes := sb1 }
      cf := { cf with methods := cf.methods.push mDef2 }

  getObjCode := Opcode.aconst_null.emit getObjCode
  getObjCode := Opcode.areturn.emit getObjCode
  let mDefGetObj : MethodDef := { accessFlags := ClassFile.ACC_PUBLIC, name := "getObj", descriptor := "(I)Llean/runtime/LeanObject;", maxStack := 2, maxLocals := 2, bytecodes := getObjCode }
  cf := { cf with methods := cf.methods.push mDefGetObj }

  setObjCode := Opcode.return_void.emit setObjCode
  let mDefSetObj : MethodDef := { accessFlags := ClassFile.ACC_PUBLIC, name := "setObj", descriptor := "(ILlean/runtime/LeanObject;)V", maxStack := 3, maxLocals := 3, bytecodes := setObjCode }
  cf := { cf with methods := cf.methods.push mDefSetObj }

  let mut getScCode := ByteArray.empty
  let mut setScCode := ByteArray.empty

  for i in [0:usize] do
    let uidx := csize + i
    let (fieldRef, cf') := cf.addFieldRef className s!"usize_{uidx}" "J"
    cf := cf'
    
    getScCode := (Opcode.iload 1).emit getScCode
    if uidx <= 127 then getScCode := (Opcode.bipush uidx.toUInt8).emit getScCode
    else getScCode := (Opcode.sipush uidx.toUInt16).emit getScCode
    let mut blk := ByteArray.empty
    blk := (Opcode.aload 0).emit blk
    blk := (Opcode.getfield fieldRef).emit blk
    blk := Opcode.lreturn.emit blk
    getScCode := (Opcode.if_icmpne (3 + blk.size).toUInt16).emit getScCode
    getScCode := getScCode ++ blk
    
    setScCode := (Opcode.iload 1).emit setScCode
    if uidx <= 127 then setScCode := (Opcode.bipush uidx.toUInt8).emit setScCode
    else setScCode := (Opcode.sipush uidx.toUInt16).emit setScCode
    let mut blk2 := ByteArray.empty
    blk2 := (Opcode.aload 0).emit blk2
    blk2 := (Opcode.lload 2).emit blk2
    blk2 := (Opcode.putfield fieldRef).emit blk2
    blk2 := Opcode.return_void.emit blk2
    setScCode := (Opcode.if_icmpne (3 + blk2.size).toUInt16).emit setScCode
    setScCode := setScCode ++ blk2
    
    if uidx == 0 then
      let mut gs0 := ByteArray.empty
      gs0 := (Opcode.aload 0).emit gs0
      gs0 := (Opcode.getfield fieldRef).emit gs0
      gs0 := Opcode.lreturn.emit gs0
      let mDef : MethodDef := { accessFlags := ClassFile.ACC_PUBLIC, name := "getScalar0", descriptor := "()J", maxStack := 2, maxLocals := 1, bytecodes := gs0 }
      cf := { cf with methods := cf.methods.push mDef }
      let mut ss0 := ByteArray.empty
      ss0 := (Opcode.aload 0).emit ss0
      ss0 := (Opcode.lload 1).emit ss0
      ss0 := (Opcode.putfield fieldRef).emit ss0
      ss0 := Opcode.return_void.emit ss0
      let mDef2 : MethodDef := { accessFlags := ClassFile.ACC_PUBLIC, name := "setScalar0", descriptor := "(J)V", maxStack := 3, maxLocals := 3, bytecodes := ss0 }
      cf := { cf with methods := cf.methods.push mDef2 }

  getScCode := Opcode.lconst_0.emit getScCode
  getScCode := Opcode.lreturn.emit getScCode
  let mDefGetSc : MethodDef := { accessFlags := ClassFile.ACC_PUBLIC, name := "getScalar", descriptor := "(I)J", maxStack := 2, maxLocals := 2, bytecodes := getScCode }
  cf := { cf with methods := cf.methods.push mDefGetSc }

  setScCode := Opcode.return_void.emit setScCode
  let mDefSetSc : MethodDef := { accessFlags := ClassFile.ACC_PUBLIC, name := "setScalar", descriptor := "(IJ)V", maxStack := 4, maxLocals := 4, bytecodes := setScCode }
  cf := { cf with methods := cf.methods.push mDefSetSc }

  if hasScalars then
    let mut getBsCode := ByteArray.empty
    let mut setBsCode := ByteArray.empty
    
    for f in layout.fieldInfo do
      if let .scalar sz offset type := f then
        let (fieldRef, cf') := cf.addFieldRef className s!"s_{offset}" (toJVMTypeDesc type)
        cf := cf'
        
        getBsCode := (Opcode.iload 1).emit getBsCode
        if offset <= 127 then getBsCode := (Opcode.bipush offset.toUInt8).emit getBsCode
        else getBsCode := (Opcode.sipush offset.toUInt16).emit getBsCode
        let mut blk := ByteArray.empty
        blk := (Opcode.aload 0).emit blk
        blk := (Opcode.getfield fieldRef).emit blk
        let desc := toJVMTypeDesc type
        if desc == "I" then blk := Opcode.i2l.emit blk
        else if desc == "F" then
          let (f2i, cf'') := cf.addMethodRef "java/lang/Float" "floatToRawIntBits" "(F)I"
          cf := cf''
          blk := (Opcode.invokestatic f2i).emit blk
          blk := Opcode.i2l.emit blk
        else if desc == "D" then
          let (d2l, cf'') := cf.addMethodRef "java/lang/Double" "doubleToRawLongBits" "(D)J"
          cf := cf''
          blk := (Opcode.invokestatic d2l).emit blk
        if desc == "I" then
          let mut maskCode := ByteArray.empty
          if sz == 1 then
            let (cIdx, cp') := cf.cp.addLong 255
            cf := { cf with cp := cp' }
            maskCode := (Opcode.ldc2_w cIdx).emit maskCode
            maskCode := Opcode.land.emit maskCode
          else if sz == 2 then
            let (cIdx, cp') := cf.cp.addLong 65535
            cf := { cf with cp := cp' }
            maskCode := (Opcode.ldc2_w cIdx).emit maskCode
            maskCode := Opcode.land.emit maskCode
          else if sz == 4 then
            let (cIdx, cp') := cf.cp.addLong 4294967295
            cf := { cf with cp := cp' }
            maskCode := (Opcode.ldc2_w cIdx).emit maskCode
            maskCode := Opcode.land.emit maskCode
          blk := blk ++ maskCode
        blk := Opcode.lreturn.emit blk
        getBsCode := (Opcode.if_icmpne (3 + blk.size).toUInt16).emit getBsCode
        getBsCode := getBsCode ++ blk

        setBsCode := (Opcode.iload 1).emit setBsCode
        if offset <= 127 then setBsCode := (Opcode.bipush offset.toUInt8).emit setBsCode
        else setBsCode := (Opcode.sipush offset.toUInt16).emit setBsCode
        let mut blk2 := ByteArray.empty
        blk2 := (Opcode.aload 0).emit blk2
        blk2 := (Opcode.lload 3).emit blk2
        if desc == "I" then blk2 := Opcode.l2i.emit blk2
        else if desc == "F" then
          blk2 := Opcode.l2i.emit blk2
          let (i2f, cf'') := cf.addMethodRef "java/lang/Float" "intBitsToFloat" "(I)F"
          cf := cf''
          blk2 := (Opcode.invokestatic i2f).emit blk2
        else if desc == "D" then
          let (l2d, cf'') := cf.addMethodRef "java/lang/Double" "longBitsToDouble" "(J)D"
          cf := cf''
          blk2 := (Opcode.invokestatic l2d).emit blk2
        blk2 := (Opcode.putfield fieldRef).emit blk2
        blk2 := Opcode.return_void.emit blk2
        setBsCode := (Opcode.if_icmpne (3 + blk2.size).toUInt16).emit setBsCode
        setBsCode := setBsCode ++ blk2
        
    getBsCode := Opcode.lconst_0.emit getBsCode
    getBsCode := Opcode.lreturn.emit getBsCode
    let mDefGetBs : MethodDef := { accessFlags := ClassFile.ACC_PUBLIC, name := "getByteScalar0", descriptor := "(II)J", maxStack := 4, maxLocals := 3, bytecodes := getBsCode }
    cf := { cf with methods := cf.methods.push mDefGetBs }

    setBsCode := Opcode.return_void.emit setBsCode
    let mDefSetBs : MethodDef := { accessFlags := ClassFile.ACC_PUBLIC, name := "setByteScalar0", descriptor := "(IIJ)V", maxStack := 4, maxLocals := 5, bytecodes := setBsCode }
    cf := { cf with methods := cf.methods.push mDefSetBs }

    let (gb0Ref, cf'') := cf.addMethodRef className "getByteScalar0" "(II)J"
    cf := cf''
    let mut gbCode := ByteArray.empty
    gbCode := (Opcode.aload 0).emit gbCode
    gbCode := (Opcode.iload 2).emit gbCode
    gbCode := (Opcode.iload 3).emit gbCode
    gbCode := (Opcode.invokevirtual gb0Ref).emit gbCode
    gbCode := Opcode.lreturn.emit gbCode
    let mDefGb : MethodDef := { accessFlags := ClassFile.ACC_PUBLIC, name := "getByteScalar", descriptor := "(III)J", maxStack := 3, maxLocals := 4, bytecodes := gbCode }
    cf := { cf with methods := cf.methods.push mDefGb }

    let (sb0Ref, cf'') := cf.addMethodRef className "setByteScalar0" "(IIJ)V"
    cf := cf''
    let mut sbCode := ByteArray.empty
    sbCode := (Opcode.aload 0).emit sbCode
    sbCode := (Opcode.iload 2).emit sbCode
    sbCode := (Opcode.iload 3).emit sbCode
    sbCode := (Opcode.lload 4).emit sbCode
    sbCode := (Opcode.invokevirtual sb0Ref).emit sbCode
    sbCode := Opcode.return_void.emit sbCode
    let mDefSb : MethodDef := { accessFlags := ClassFile.ACC_PUBLIC, name := "setByteScalar", descriptor := "(IIIJ)V", maxStack := 5, maxLocals := 6, bytecodes := sbCode }
    cf := { cf with methods := cf.methods.push mDefSb }

    if usize == 0 then
      let mut gs0 := ByteArray.empty
      gs0 := (Opcode.aload 0).emit gs0
      gs0 := Opcode.iconst_0.emit gs0
      gs0 := (Opcode.bipush 8).emit gs0
      gs0 := (Opcode.invokevirtual gb0Ref).emit gs0
      gs0 := Opcode.lreturn.emit gs0
      let mDefGs0 : MethodDef := { accessFlags := ClassFile.ACC_PUBLIC, name := "getScalar0", descriptor := "()J", maxStack := 3, maxLocals := 1, bytecodes := gs0 }
      cf := { cf with methods := cf.methods.push mDefGs0 }

      let mut ss0 := ByteArray.empty
      ss0 := (Opcode.aload 0).emit ss0
      ss0 := Opcode.iconst_0.emit ss0
      ss0 := (Opcode.bipush 8).emit ss0
      ss0 := (Opcode.lload 1).emit ss0
      ss0 := (Opcode.invokevirtual sb0Ref).emit ss0
      ss0 := Opcode.return_void.emit ss0
      let mDefSs0 : MethodDef := { accessFlags := ClassFile.ACC_PUBLIC, name := "setScalar0", descriptor := "(J)V", maxStack := 5, maxLocals := 3, bytecodes := ss0 }
      cf := { cf with methods := cf.methods.push mDefSs0 }

  modify fun s => { s with syntheticClasses := s.syntheticClasses.push cf }
  return className
def shouldEmitCtorClass (info : CtorInfo) : EmitJVMM Bool := do
  let numScalars := info.usize + (info.ssize + 7) / 8
  if info.size > 2 || numScalars > 0 then return true
  let env ← getEnv
  match env.find? info.name with
  | some (.ctorInfo val) =>
    if isStructure env val.induct then
      return true
    return false
  | _ => return false

def emitCtor (info : CtorInfo) (args : Array (Arg .impure)) : EmitJVMM Unit := do
  if ← shouldEmitCtorClass info then
    let layout? ← try
        let l ← getCtorLayout info.name
        pure (some l)
      catch _ => pure none
    if let some layout := layout? then
      let className ← emitCtorClass info.name layout
      let classIdx ← addClass className
      emitOp (.new classIdx)
      emitOp .dup
      for arg in args do
        emitCtorArg arg
      let mut initDesc := "("
      for _ in [0:info.size] do
        initDesc := initDesc ++ "Llean/runtime/LeanObject;"
      initDesc := initDesc ++ ")V"
      let methodIdx ← addMethodRef className "<init>" initDesc
      emitOp (.invokespecial methodIdx)
      return ()
  
  let numScalars := info.usize + (info.ssize + 7) / 8
  if info.size == 0 && numScalars == 0 then
    emitPushInt info.cidx
    let alloc0Idx ← addMethodRef "lean/runtime/LeanCtor" "alloc0" "(I)Llean/runtime/LeanCtor;"
    emitOp (.invokestatic alloc0Idx)
  else if info.size == 1 && numScalars == 0 then
    emitPushInt info.cidx
    if h : 0 < args.size then
      emitCtorArg args[0]
    else
      emitOp .aconst_null
    let alloc1Idx ← addMethodRef "lean/runtime/LeanCtor" "alloc1" "(ILlean/runtime/LeanObject;)Llean/runtime/LeanCtor;"
    emitOp (.invokestatic alloc1Idx)
  else if info.size == 2 && numScalars == 0 then
    emitPushInt info.cidx
    if h0 : 0 < args.size then
      emitCtorArg args[0]
    else
      emitOp .aconst_null
    if h1 : 1 < args.size then
      emitCtorArg args[1]
    else
      emitOp .aconst_null
    let alloc2Idx ← addMethodRef "lean/runtime/LeanCtor" "alloc2" "(ILlean/runtime/LeanObject;Llean/runtime/LeanObject;)Llean/runtime/LeanCtor;"
    emitOp (.invokestatic alloc2Idx)
  else if info.size == 0 && numScalars == 1 then
    emitPushInt info.cidx
    emitOp .lconst_0
    let allocScalar1Idx ← addMethodRef "lean/runtime/LeanCtor" "allocScalar1" "(IJ)Llean/runtime/LeanCtor;"
    emitOp (.invokestatic allocScalar1Idx)
  else
    emitPushInt info.cidx
    emitPushInt info.size
    emitPushInt numScalars
    let allocIdx ← addMethodRef "lean/runtime/LeanCtor" "alloc" "(III)Llean/runtime/LeanCtor;"
    emitOp (.invokestatic allocIdx)
    if args.size > 0 then
      let setObjIdx ← addMethodRef "lean/runtime/LeanCtor" "setObj" "(ILlean/runtime/LeanObject;)V"
      for h : i in 0...args.size do
        let arg := args[i]
        emitOp .dup
        emitPushInt i
        emitCtorArg arg
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
  | ``UInt64.decEq | ``USize.decEq =>
    if !isLongArg ctx args[0]! || !isLongArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp .lcmp
    emitOp (.ifeq 7)
    emitOp .iconst_0
    emitOp (.goto 4)
    emitOp .iconst_1
    return true
  | ``UInt64.decLt | ``USize.decLt =>
    if !isLongArg ctx args[0]! || !isLongArg ctx args[1]! then return false
    emitBinaryArgs args
    let ref ← addMethodRef "java/lang/Long" "compareUnsigned" "(JJ)I"
    emitOp (.invokestatic ref)
    emitOp (.iflt 7)
    emitOp .iconst_0
    emitOp (.goto 4)
    emitOp .iconst_1
    return true
  | ``UInt64.decLe | ``USize.decLe =>
    if !isLongArg ctx args[0]! || !isLongArg ctx args[1]! then return false
    emitBinaryArgs args
    let ref ← addMethodRef "java/lang/Long" "compareUnsigned" "(JJ)I"
    emitOp (.invokestatic ref)
    emitOp (.ifle 7)
    emitOp .iconst_0
    emitOp (.goto 4)
    emitOp .iconst_1
    return true
  | ``UInt32.decEq =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp (.if_icmpeq 7)
    emitOp .iconst_0
    emitOp (.goto 4)
    emitOp .iconst_1
    return true
  | ``UInt32.decLt =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    let ref ← addMethodRef "java/lang/Integer" "compareUnsigned" "(II)I"
    emitOp (.invokestatic ref)
    emitOp (.iflt 7)
    emitOp .iconst_0
    emitOp (.goto 4)
    emitOp .iconst_1
    return true
  | ``UInt32.decLe =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    let ref ← addMethodRef "java/lang/Integer" "compareUnsigned" "(II)I"
    emitOp (.invokestatic ref)
    emitOp (.ifle 7)
    emitOp .iconst_0
    emitOp (.goto 4)
    emitOp .iconst_1
    return true
  | ``UInt16.decEq | ``UInt8.decEq =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp (.if_icmpeq 7)
    emitOp .iconst_0
    emitOp (.goto 4)
    emitOp .iconst_1
    return true
  | ``UInt16.decLt | ``UInt8.decLt =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp (.if_icmplt 7)
    emitOp .iconst_0
    emitOp (.goto 4)
    emitOp .iconst_1
    return true
  | ``UInt16.decLe | ``UInt8.decLe =>
    if !isIntArg ctx args[0]! || !isIntArg ctx args[1]! then return false
    emitBinaryArgs args
    emitOp (.if_icmple 7)
    emitOp .iconst_0
    emitOp (.goto 4)
    emitOp .iconst_1
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
    let isBoxed := isBoxedName fn
    let localDecl? := (← read).localDecls.find? (·.name == fn)
    let totalArity? := match localDecl? with
      | some decl => some decl.params.size
      | none => sig?.map fun (sig : Signature .impure) => sig.params.size
    let totalArity := totalArity?.getD args.size

    let (paramTypes, retDesc) := match localDecl? with
      | some decl =>
        if isBoxed then
          (decl.params.map fun _ => ImpureType.object, leanObjTypeDesc)
        else
          (decl.params.map fun (p : Param .impure) => p.type, toJVMTypeDesc decl.type)
      | none => match sig? with
        | some sig =>
          if isBoxed then
            (sig.params.map fun _ => ImpureType.object, leanObjTypeDesc)
          else
            (sig.params.map fun (p : Param .impure) => p.type, toJVMTypeDesc sig.type)
        | none =>
          ((List.replicate totalArity ImpureType.object).toArray, leanObjTypeDesc)
    let mut pDescs := ""
    for p in paramTypes do
      pDescs := pDescs ++ toJVMTypeDesc p
    if paramTypes.size < totalArity then
      for _ in List.range (totalArity - paramTypes.size) do
        pDescs := pDescs ++ leanObjTypeDesc
    let targetDesc := s!"({pDescs}){retDesc}"

    let rem := if totalArity > args.size then totalArity - args.size else 0

    -- 1. Push captured arguments onto the operand stack
    for arg in args do
      match arg with
      | .fvar argId => emitLoadAs argId ImpureType.object
      | .erased => emitOp .aconst_null

    -- 2. Register target implementation method reference & MethodHandle (REF_invokeStatic = 6)
    let targetMethodRef ← addMethodRef targetClass methodName targetDesc
    let targetHandleIdx ← addMethodHandle 6 targetMethodRef

    -- 3. Register standard java.lang.invoke.LambdaMetafactory.metafactory bootstrap method
    let bsMethodDesc := "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"
    let bsMethodRef ← addMethodRef "java/lang/invoke/LambdaMetafactory" "metafactory" bsMethodDesc
    let bsHandleIdx ← addMethodHandle 6 bsMethodRef

    -- 4. SAM interface method descriptor: (rem LeanObjects) -> LeanObject
    let mut samParamDescs := ""
    for _ in List.range rem do
      samParamDescs := samParamDescs ++ leanObjTypeDesc
    let samDesc := s!"({samParamDescs}){leanObjTypeDesc}"
    let samMethodTypeIdx ← addMethodType samDesc

    -- 5. Register BootstrapMethod attribute entry: [samMethodType, implMethod, instantiatedMethodType]
    let bmAttrIdx ← addBootstrapMethod bsHandleIdx #[samMethodTypeIdx, targetHandleIdx, samMethodTypeIdx]

    -- 6. Construct callsite descriptor: (captured...) -> LeanFn{rem}
    let fnInterface := s!"lean/runtime/LeanFn{rem}"
    let mut callsiteParamDescs := ""
    for _ in List.range args.size do
      callsiteParamDescs := callsiteParamDescs ++ leanObjTypeDesc
    let callsiteDesc := s!"({callsiteParamDescs})L{fnInterface};"

    -- 7. Register CONSTANT_InvokeDynamic and emit opcode
    let indyIdx ← addInvokeDynamic bmAttrIdx "invoke" callsiteDesc
    emitOp (.invokedynamic indyIdx)

    -- 8. Wrap into LeanClosure via LeanClosure.ofFn{rem}
    let ofFnMethodName := s!"ofFn{rem}"
    let ofFnDesc := s!"(L{fnInterface};){leanClosureTypeDesc}"
    let ofFnRef ← addMethodRef "lean/runtime/LeanClosure" ofFnMethodName ofFnDesc
    emitOp (.invokestatic ofFnRef)
  | .ctor info args =>
    emitCtor info args
    | .oproj i fvarId =>
    let ctx ← read
    if let some (ctorName, layout) := ctx.ctorClassMap[fvarId]? then
      let className := "lean/ctor_" ++ ctorName.mangle
      let classIdx ← addClass className
      emitLoad fvarId
      emitOp (.checkcast classIdx)
      let fieldRef ← addFieldRef className s!"obj_{i}" "Llean/runtime/LeanObject;"
      emitOp (.getfield fieldRef)
      return ()
    emitLoad fvarId
    let ctorClassIdx ← addClass "lean/runtime/LeanCtor"
    emitOp (.checkcast ctorClassIdx)
    if i == 0 then
      let getObj0Idx ← addMethodRef "lean/runtime/LeanCtor" "getObj0" "()Llean/runtime/LeanObject;"
      emitOp (.invokevirtual getObj0Idx)
    else if i == 1 then
      let getObj1Idx ← addMethodRef "lean/runtime/LeanCtor" "getObj1" "()Llean/runtime/LeanObject;"
      emitOp (.invokevirtual getObj1Idx)
    else
      emitPushInt i
      let getObjIdx ← addMethodRef "lean/runtime/LeanCtor" "getObj" "(I)Llean/runtime/LeanObject;"
      emitOp (.invokevirtual getObjIdx)
    | .uproj i fvarId =>
    let ctx ← read
    if let some (ctorName, layout) := ctx.ctorClassMap[fvarId]? then
      let className := "lean/ctor_" ++ ctorName.mangle
      let classIdx ← addClass className
      emitLoad fvarId
      emitOp (.checkcast classIdx)
      let fieldRef ← addFieldRef className s!"usize_{i}" "J"
      emitOp (.getfield fieldRef)
      if isScalarType decl.type then
        if toJVMTypeDesc decl.type == "I" then
          emitOp .l2i
      else
        let ofLongIdx ← addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
        emitOp (.invokestatic ofLongIdx)
      return ()
    emitLoad fvarId
    let ctorClassIdx ← addClass "lean/runtime/LeanCtor"
    emitOp (.checkcast ctorClassIdx)
    if i == 0 then
      let getScalar0Idx ← addMethodRef "lean/runtime/LeanCtor" "getScalar0" "()J"
      emitOp (.invokevirtual getScalar0Idx)
    else
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
    let ctx ← read
    if let some (ctorName, layout) := ctx.ctorClassMap[fvarId]? then
      let className := "lean/ctor_" ++ ctorName.mangle
      let classIdx ← addClass className
      emitLoad fvarId
      emitOp (.checkcast classIdx)
      let fieldRef ← addFieldRef className s!"s_{offset}" (toJVMTypeDesc decl.type)
      emitOp (.getfield fieldRef)
      -- Wait, if the JVM field is an int, but we want an int, getfield already gives an int!
      -- We don't need any casting or masking!
      -- Except for `LeanNat.ofLong` if it's a generic object but wait, scalar fields in Lean 4 structs are always unboxed, so decl.type is scalar!
      -- Actually, `EmitJVM` checks if it's NOT a scalar type and boxes it using `LeanNat.ofLong`. Wait, scalar struct fields can't be generic LeanNat.
      -- If `toJVMTypeDesc decl.type == "I"`, `getfield` gives "I". We don't need `l2i`.
      -- If `decl.type == ImpureType.float`, `getfield` gives "D". No conversion needed!
      return ()
    emitLoad fvarId
    let ctorClassIdx ← addClass "lean/runtime/LeanCtor"
    emitOp (.checkcast ctorClassIdx)
    let numBytes := getScalarNumBytes decl.type
    if offset == 0 && numBytes == 8 then
      let getScalar0Idx ← addMethodRef "lean/runtime/LeanCtor" "getScalar0" "()J"
      emitOp (.invokevirtual getScalar0Idx)
    else if offset < 8 then
      emitPushInt offset
      emitPushInt numBytes
      let getByteScalar0Idx ← addMethodRef "lean/runtime/LeanCtor" "getByteScalar0" "(II)J"
      emitOp (.invokevirtual getByteScalar0Idx)
    else
      emitPushInt n
      emitPushInt offset
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
Detects if code is a self-tail call returning its result.
-/
def isTailCall (code : Code .impure) : EmitJVMM Bool := do
  match code with
  | .let { fvarId := fvarId, value := .fap declName _, .. } k =>
    let rec checkReturn (k : Code .impure) : Bool :=
      match k with
      | .return fvarId' => fvarId == fvarId'
      | .inc (k := k') .. | .dec (k := k') .. | .del (k := k') .. => checkReturn k'
      | _ => false
    return checkReturn k && (← read).currFn == declName
  | _ => return false

def paramEqArg (p : Param .impure) (arg : Arg .impure) : Bool :=
  match arg with
  | .fvar fvarId => p.fvarId == fvarId
  | .erased => false

def overwriteParam (ps : Array (Param .impure)) (args : Array (Arg .impure)) : Bool := Id.run do
  for h1 : i in 0...ps.size do
    let p := ps[i]
    for h2 : j in (i+1)...args.size do
      if paramEqArg p args[j] then
        return true
  return false

def emitArgAs (arg : Arg .impure) (ty : Expr) : EmitJVMM Unit := do
  match arg with
  | .fvar id => emitLoadAs id ty
  | .erased =>
    if toJVMTypeDesc ty == "I" then emitOp .iconst_0
    else if toJVMTypeDesc ty == "J" then emitOp .lconst_0
    else emitOp .aconst_null

partial def collectJoinPoints (code : Code .impure) : Array (FunDecl .impure) :=
  go code #[]
where
  go (code : Code .impure) (acc : Array (FunDecl .impure)) : Array (FunDecl .impure) :=
    match code with
    | .let _ k => go k acc
    | .jp decl k =>
      let acc := acc.push decl
      let acc := go decl.value acc
      go k acc
    | .cases cs =>
      cs.alts.foldl (fun a alt => go alt.getCode a) acc
    | .inc _ _ _ _ k | .dec _ _ _ _ _ k | .del _ k | .setTag _ _ k
    | .oset _ _ _ k | .uset _ _ _ k | .sset _ _ _ _ _ k => go k acc
    | .return _ | .jmp _ _ | .unreach _ => acc

def isTerminalBytecode (code : ByteArray) : Bool :=
  if code.size == 0 then
    false
  else
    let lastByte := code.get! (code.size - 1)
    if (lastByte >= 0xac && lastByte <= 0xb1) || lastByte == 0xbf then
      true
    else if code.size >= 3 && code.get! (code.size - 3) == 0xa7 then
      true
    else
      false

/--
Emits a self-tail call by assigning new arguments into parameter slots and jumping back to offset 0.
-/
def emitTailCall (decl : LetDecl .impure) : EmitJVMM Unit := do
  let .fap _ args := decl.value | unreachable!
  let ctx ← read
  let ps := ctx.currParams

  if overwriteParam ps args then
    let mut tmpSlots : Array (Option UInt8) := #[]
    let mut curSlot := ctx.nextSlot
    for h : i in 0...ps.size do
      let p := ps[i]
      let arg := if h2 : i < args.size then args[i] else .erased
      if !paramEqArg p arg then
        let pTy := ctx.varTypeMap[p.fvarId]?.getD p.type
        tmpSlots := tmpSlots.push (some curSlot)
        curSlot := curSlot + getSlotSize pTy
      else
        tmpSlots := tmpSlots.push none
    for h : i in 0...ps.size do
      if let some tmpSlot := tmpSlots[i]! then
        let p := ps[i]
        let arg := if h2 : i < args.size then args[i] else .erased
        let pTy := ctx.varTypeMap[p.fvarId]?.getD p.type
        emitArgAs arg pTy
        emitStoreTyped tmpSlot pTy
    for h : i in 0...ps.size do
      if let some tmpSlot := tmpSlots[i]! then
        let p := ps[i]
        let pTy := ctx.varTypeMap[p.fvarId]?.getD p.type
        let pSlot := ctx.varSlotMap[p.fvarId]?.getD 0
        match toJVMTypeDesc pTy with
        | "I" => emitOp (.iload tmpSlot); emitOp (.istore pSlot)
        | "J" => emitOp (.lload tmpSlot); emitOp (.lstore pSlot)
        | "F" => emitOp (.fload tmpSlot); emitOp (.fstore pSlot)
        | "D" => emitOp (.dload tmpSlot); emitOp (.dstore pSlot)
        | _   => emitOp (.aload tmpSlot); emitOp (.astore pSlot)
  else
    for h : i in 0...ps.size do
      let p := ps[i]
      let arg := if h2 : i < args.size then args[i] else .erased
      unless paramEqArg p arg do
        let pTy := ctx.varTypeMap[p.fvarId]?.getD p.type
        let pSlot := ctx.varSlotMap[p.fvarId]?.getD 0
        emitArgAs arg pTy
        emitStoreTyped pSlot pTy
  let fpc := (← get).code.size
  emitOp (.goto (0x10000 - fpc).toUInt16)
  modify fun s => { s with fixups := s.fixups.push (fpc, `_start) }

/--
Emits a basic block of LCNF code to JVM bytecode.
-/
partial def emitCode (code : Code .impure) : EmitJVMM Unit := do
  match code with
  | .let decl k =>
    if ← isTailCall code then
      emitTailCall decl
    else
      emitLetValue decl
      let actualType := match decl.value with
        | .ctor .. | .box .. | .lit (.nat ..) => ImpureType.object
        | _ => decl.type
      let (slot, newCtx') ← allocSlot decl.fvarId actualType
      let mut newCtx := newCtx'
      match decl.value with
      | .ctor info _ | .reuse _ info _ _ =>
        if ← shouldEmitCtorClass info then
          let layout? ← try
              let l ← getCtorLayout info.name
              pure (some l)
            catch _ => pure none
          if let some layout := layout? then
            newCtx := { newCtx with ctorClassMap := newCtx.ctorClassMap.insert decl.fvarId (info.name, layout) }
      | _ => pure ()
      
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
      let canUseTableSwitch :=
        if cs.alts.size >= 3 then
          let hasDefault := cs.alts.any fun
            | .default .. => true
            | _ => false
          if hasDefault then
            false
          else
            let cidxs := cs.alts.filterMap fun
              | .ctorAlt info _ => some info.cidx
              | _ => none
            if cidxs.size == cs.alts.size then
              let sortedCidxs := cidxs.qsort (· < ·)
              Id.run do
                let mut isDense := true
                for h : i in 0...sortedCidxs.size do
                  if sortedCidxs[i] != i then
                    isDense := false
                return isDense
            else
              false
        else
          false

      if canUseTableSwitch then
        let sortedAlts := cs.alts.qsort fun a b =>
          match a, b with
          | .ctorAlt info1 _, .ctorAlt info2 _ => info1.cidx < info2.cidx
          | _, _ => false
        let discrTy := ctx.varTypeMap[cs.discr]?.getD ImpureType.object
        if toJVMTypeDesc discrTy == leanObjTypeDesc then
          emitLoad cs.discr
          let getTagIdx ← addMethodRef "lean/runtime/LeanObject" "getTag" "()I"
          emitOp (.invokevirtual getTagIdx)
        else
          emitLoad cs.discr
          if toJVMTypeDesc discrTy == "J" then
            emitOp .l2i

        let mut branchCodes : Array (ByteArray × Array (Nat × Name) × Array (Name × Nat)) := #[]
        for alt in sortedAlts do
          match alt with
          | .ctorAlt _ k =>
            let triple ← captureCode (emitCode k)
            branchCodes := branchCodes.push triple
          | _ => unreachable!
        let retType := (← read).currReturnType
        let (defaultCode, defaultFixups, defaultLabels) ← captureCode (emitDefaultReturn retType)

        let switchPc := (← get).code.size
        let pad := (4 - ((switchPc + 1) % 4)) % 4
        let tableswitchLen := 1 + pad + 4 + 4 + 4 + sortedAlts.size * 4
        let mut curOffset := tableswitchLen
        let mut offsets : Array UInt32 := #[]
        for (bCode, _, _) in branchCodes do
          offsets := offsets.push curOffset.toUInt32
          curOffset := curOffset + bCode.size
        let defOffset := curOffset.toUInt32
        emitOp (.tableswitch 0 (sortedAlts.size - 1).toUInt32 defOffset offsets)
        for (bCode, bFixups, bLabels) in branchCodes do
          appendCode bCode bFixups bLabels
        appendCode defaultCode defaultFixups defaultLabels
      else
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
              let (altCode, altFixups, altLabels) ← captureCode (emitCode k)
              emitOp (.iload tagSlot)
              emitPushInt info.cidx
              let jumpOffset := (3 + altCode.size).toUInt16
              emitOp (.if_icmpne jumpOffset)
              appendCode altCode altFixups altLabels
            | .default k =>
              emitCode k
          if !hasDefault then
            let retType := (← read).currReturnType
            emitDefaultReturn retType
  | .jmp fvarId args =>
    let ctx ← read
    match ctx.joinPoints[fvarId]? with
    | some decl =>
      let ps := decl.params
      if overwriteParam ps args then
        let mut curSlot := ctx.nextSlot
        let mut tmpSlots : Array (Option UInt8) := #[]
        for h : i in 0...ps.size do
          let p := ps[i]
          let arg := if h2 : i < args.size then args[i] else .erased
          if !paramEqArg p arg then
            let pTy := ctx.varTypeMap[p.fvarId]?.getD p.type
            tmpSlots := tmpSlots.push (some curSlot)
            curSlot := curSlot + getSlotSize pTy
          else
            tmpSlots := tmpSlots.push none
        for h : i in 0...ps.size do
          if let some tmpSlot := tmpSlots[i]! then
            let p := ps[i]
            let arg := if h2 : i < args.size then args[i] else .erased
            let pTy := ctx.varTypeMap[p.fvarId]?.getD p.type
            emitArgAs arg pTy
            emitStoreTyped tmpSlot pTy
        for h : i in 0...ps.size do
          if let some tmpSlot := tmpSlots[i]! then
            let p := ps[i]
            let pTy := ctx.varTypeMap[p.fvarId]?.getD p.type
            let pSlot := ctx.varSlotMap[p.fvarId]?.getD 0
            match toJVMTypeDesc pTy with
            | "I" => emitOp (.iload tmpSlot); emitOp (.istore pSlot)
            | "J" => emitOp (.lload tmpSlot); emitOp (.lstore pSlot)
            | "F" => emitOp (.fload tmpSlot); emitOp (.fstore pSlot)
            | "D" => emitOp (.dload tmpSlot); emitOp (.dstore pSlot)
            | _   => emitOp (.aload tmpSlot); emitOp (.astore pSlot)
      else
        for h : i in 0...ps.size do
          let p := ps[i]
          let arg := if h2 : i < args.size then args[i] else .erased
          unless paramEqArg p arg do
            let pTy := ctx.varTypeMap[p.fvarId]?.getD p.type
            let pSlot := ctx.varSlotMap[p.fvarId]?.getD 0
            emitArgAs arg pTy
            emitStoreTyped pSlot pTy
      let fpc := (← get).code.size
      emitOp (.goto 0)
      modify fun s => { s with fixups := s.fixups.push (fpc, fvarId.name) }
    | none =>
      let retType := ctx.currReturnType
      emitDefaultReturn retType
  | .jp decl k =>
    emitCode k
    if (← get).fixups.any (·.2 == decl.fvarId.name) then
      let offset := (← get).code.size
      modify fun s => { s with labelOffsets := s.labelOffsets.insert decl.fvarId.name offset }
      emitCode decl.value
      let retType := (← read).currReturnType
      if !isTerminalBytecode (← get).code then
        emitDefaultReturn retType
  | .inc (k := k) .. | .dec (k := k) .. | .del (k := k) .. =>
    -- Tracing GC: Reference counting instructions are no-ops!
    emitCode k
    | .oset fvarId i y k =>
    let ctx ← read
    if let some (ctorName, layout) := ctx.ctorClassMap[fvarId]? then
      let className := "lean/ctor_" ++ ctorName.mangle
      let classIdx ← addClass className
      emitLoad fvarId
      emitOp (.checkcast classIdx)
      match y with
      | .fvar yId => emitLoadAs yId ImpureType.object
      | .erased => emitOp .aconst_null
      let fieldRef ← addFieldRef className s!"obj_{i}" "Llean/runtime/LeanObject;"
      emitOp (.putfield fieldRef)
      emitCode k
      return ()
    emitLoad fvarId
    let ctorClassIdx ← addClass "lean/runtime/LeanCtor"
    emitOp (.checkcast ctorClassIdx)
    if i == 0 then
      match y with
      | .fvar yId => emitLoadAs yId ImpureType.object
      | .erased => emitOp .aconst_null
      let setObj0Idx ← addMethodRef "lean/runtime/LeanCtor" "setObj0" "(Llean/runtime/LeanObject;)V"
      emitOp (.invokevirtual setObj0Idx)
    else if i == 1 then
      match y with
      | .fvar yId => emitLoadAs yId ImpureType.object
      | .erased => emitOp .aconst_null
      let setObj1Idx ← addMethodRef "lean/runtime/LeanCtor" "setObj1" "(Llean/runtime/LeanObject;)V"
      emitOp (.invokevirtual setObj1Idx)
    else
      emitPushInt i
      match y with
      | .fvar yId => emitLoadAs yId ImpureType.object
      | .erased => emitOp .aconst_null
      let setObjIdx ← addMethodRef "lean/runtime/LeanCtor" "setObj" "(ILlean/runtime/LeanObject;)V"
      emitOp (.invokevirtual setObjIdx)
    emitCode k
    | .uset fvarId i y k =>
    let ctx ← read
    if let some (ctorName, layout) := ctx.ctorClassMap[fvarId]? then
      let className := "lean/ctor_" ++ ctorName.mangle
      let classIdx ← addClass className
      emitLoad fvarId
      emitOp (.checkcast classIdx)
      emitLoad y
      let yType := ctx.varTypeMap[y]?.getD ImpureType.object
      if toJVMTypeDesc yType == "J" then pure ()
      else if toJVMTypeDesc yType == "I" then emitOp .i2l
      else
        let getScalar64Idx ← addMethodRef "lean/runtime/LeanRuntimeJVM" "getScalar64" "(Llean/runtime/LeanObject;)J"
        emitOp (.invokestatic getScalar64Idx)
      let fieldRef ← addFieldRef className s!"usize_{i}" "J"
      emitOp (.putfield fieldRef)
      emitCode k
      return ()
    emitLoad fvarId
    let ctorClassIdx ← addClass "lean/runtime/LeanCtor"
    emitOp (.checkcast ctorClassIdx)
    let ctx ← read
    let yType := ctx.varTypeMap[y]?.getD ImpureType.object
    if i == 0 then
      emitLoad y
      if toJVMTypeDesc yType == "J" then
        pure ()
      else if toJVMTypeDesc yType == "I" then
        emitOp .i2l
      else
        let getScalar64Idx ← addMethodRef "lean/runtime/LeanRuntimeJVM" "getScalar64" "(Llean/runtime/LeanObject;)J"
        emitOp (.invokestatic getScalar64Idx)
      let setScalar0Idx ← addMethodRef "lean/runtime/LeanCtor" "setScalar0" "(J)V"
      emitOp (.invokevirtual setScalar0Idx)
    else
      emitPushInt i
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
    let ctx ← read
    if let some (ctorName, layout) := ctx.ctorClassMap[fvarId]? then
      let className := "lean/ctor_" ++ ctorName.mangle
      let classIdx ← addClass className
      emitLoad fvarId
      emitOp (.checkcast classIdx)
      emitLoad y
      let yType := ctx.varTypeMap[y]?.getD ty
      let destTypeDesc := toJVMTypeDesc ty
      -- The value `y` has `yType`, and the field has `destTypeDesc`.
      -- If they differ, we cast `y` to `destTypeDesc`. But usually `yType` == `ty`, except if `yType` is `object`.
      -- If `yType` is object (boxed scalar), we must unbox it.
      if destTypeDesc == "I" then
        if toJVMTypeDesc yType == "I" then pure ()
        else if toJVMTypeDesc yType == "J" then emitOp .l2i
        else if toJVMTypeDesc yType == "D" then
          let d2lIdx ← addMethodRef "java/lang/Double" "doubleToRawLongBits" "(D)J"
          emitOp (.invokestatic d2lIdx)
          emitOp .l2i
        else if toJVMTypeDesc yType == "F" then
          let f2iIdx ← addMethodRef "java/lang/Float" "floatToRawIntBits" "(F)I"
          emitOp (.invokestatic f2iIdx)
        else
          let getScalar64Idx ← addMethodRef "lean/runtime/LeanRuntimeJVM" "getScalar64" "(Llean/runtime/LeanObject;)J"
          emitOp (.invokestatic getScalar64Idx)
          emitOp .l2i
      else if destTypeDesc == "J" then
        if toJVMTypeDesc yType == "I" then emitOp .i2l
        else if toJVMTypeDesc yType == "J" then pure ()
        else if toJVMTypeDesc yType == "D" then
          let d2lIdx ← addMethodRef "java/lang/Double" "doubleToRawLongBits" "(D)J"
          emitOp (.invokestatic d2lIdx)
        else
          let getScalar64Idx ← addMethodRef "lean/runtime/LeanRuntimeJVM" "getScalar64" "(Llean/runtime/LeanObject;)J"
          emitOp (.invokestatic getScalar64Idx)
      else if destTypeDesc == "D" then
        if toJVMTypeDesc yType == "D" then pure ()
        else if toJVMTypeDesc yType == "J" then
          let l2dIdx ← addMethodRef "java/lang/Double" "longBitsToDouble" "(J)D"
          emitOp (.invokestatic l2dIdx)
        else if toJVMTypeDesc yType == "I" then
          emitOp .i2l
          let l2dIdx ← addMethodRef "java/lang/Double" "longBitsToDouble" "(J)D"
          emitOp (.invokestatic l2dIdx)
        else
          let getScalar64Idx ← addMethodRef "lean/runtime/LeanRuntimeJVM" "getScalar64" "(Llean/runtime/LeanObject;)J"
          emitOp (.invokestatic getScalar64Idx)
          let l2dIdx ← addMethodRef "java/lang/Double" "longBitsToDouble" "(J)D"
          emitOp (.invokestatic l2dIdx)
      else if destTypeDesc == "F" then
        if toJVMTypeDesc yType == "F" then pure ()
        else if toJVMTypeDesc yType == "I" then
          let i2fIdx ← addMethodRef "java/lang/Float" "intBitsToFloat" "(I)F"
          emitOp (.invokestatic i2fIdx)
        else if toJVMTypeDesc yType == "J" then
          emitOp .l2i
          let i2fIdx ← addMethodRef "java/lang/Float" "intBitsToFloat" "(I)F"
          emitOp (.invokestatic i2fIdx)
        else
          let getScalar64Idx ← addMethodRef "lean/runtime/LeanRuntimeJVM" "getScalar64" "(Llean/runtime/LeanObject;)J"
          emitOp (.invokestatic getScalar64Idx)
          emitOp .l2i
          let i2fIdx ← addMethodRef "java/lang/Float" "intBitsToFloat" "(I)F"
          emitOp (.invokestatic i2fIdx)
      
      let fieldRef ← addFieldRef className s!"s_{offset}" destTypeDesc
      emitOp (.putfield fieldRef)
      emitCode k
      return ()
    emitLoad fvarId
    let ctorClassIdx ← addClass "lean/runtime/LeanCtor"
    emitOp (.checkcast ctorClassIdx)
    let numBytes := getScalarNumBytes ty
    let ctx ← read
    let yType := ctx.varTypeMap[y]?.getD ty
    if offset == 0 && numBytes == 8 then
      emitLoad y
      if toJVMTypeDesc yType == "I" then
        emitOp .i2l
      else if toJVMTypeDesc yType == "D" then
        let toRawBitsIdx ← addMethodRef "java/lang/Double" "doubleToRawLongBits" "(D)J"
        emitOp (.invokestatic toRawBitsIdx)
      else if toJVMTypeDesc yType != "J" then
        let getScalar64Idx ← addMethodRef "lean/runtime/LeanRuntimeJVM" "getScalar64" "(Llean/runtime/LeanObject;)J"
        emitOp (.invokestatic getScalar64Idx)
      let setScalar0Idx ← addMethodRef "lean/runtime/LeanCtor" "setScalar0" "(J)V"
      emitOp (.invokevirtual setScalar0Idx)
    else if offset < 8 then
      emitPushInt offset
      emitPushInt numBytes
      emitLoad y
      if toJVMTypeDesc yType == "I" then
        emitOp .i2l
      else if toJVMTypeDesc yType == "D" then
        let toRawBitsIdx ← addMethodRef "java/lang/Double" "doubleToRawLongBits" "(D)J"
        emitOp (.invokestatic toRawBitsIdx)
      else if toJVMTypeDesc yType != "J" then
        let getScalar64Idx ← addMethodRef "lean/runtime/LeanRuntimeJVM" "getScalar64" "(Llean/runtime/LeanObject;)J"
        emitOp (.invokestatic getScalar64Idx)
      let setByteScalar0Idx ← addMethodRef "lean/runtime/LeanCtor" "setByteScalar0" "(IIJ)V"
      emitOp (.invokevirtual setByteScalar0Idx)
    else
      emitPushInt i
      emitPushInt offset
      emitPushInt numBytes
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
def getStructCtorLayout? (monoType : Expr) : EmitJVMM (Option (Name × CtorLayout)) := do
  let env ← getEnv
  let indName := monoType.getAppFn.constName?
  if let some indName := indName then
    if (← hasTrivialImpureStructure? indName).isNone then
      if let some (.inductInfo val) := env.find? indName then
        if val.ctors.length == 1 then
          let ctorName := val.ctors[0]!
          let layout? ← try
              let l ← getCtorLayout ctorName
              pure (some l)
            catch _ => pure none
          if let some layout := layout? then
            if ← shouldEmitCtorClass layout.ctorInfo then
              discard <| emitCtorClass ctorName layout
              return some (ctorName, layout)
  return none

partial def collectStructTypes (code : Code .pure) (acc : Std.HashMap FVarId (Name × CtorLayout)) : EmitJVMM (Std.HashMap FVarId (Name × CtorLayout)) := do
  match code with
  | .let decl k =>
    let mut acc := acc
    if let some entry ← getStructCtorLayout? decl.type then
      acc := acc.insert decl.fvarId entry
    collectStructTypes k acc
  | .fun decl k | .jp decl k =>
    let mut acc := acc
    if let some entry ← getStructCtorLayout? decl.type then
      acc := acc.insert decl.fvarId entry
    for p in decl.params do
      if let some entry ← getStructCtorLayout? p.type then
        acc := acc.insert p.fvarId entry
    acc ← collectStructTypes decl.value acc
    collectStructTypes k acc
  | .cases cs =>
    let mut acc := acc
    for alt in cs.alts do
      acc ← collectStructTypes alt.getCode acc
    return acc
  | _ => return acc

def populateCtorClassMap (decl : Decl .impure) : EmitJVMM (Std.HashMap FVarId (Name × CtorLayout)) := do
  let mut acc : Std.HashMap FVarId (Name × CtorLayout) := {}
  if let some monoDecl ← getMonoDecl? decl.name then
    for p in monoDecl.params do
      if let some entry ← getStructCtorLayout? p.type then
        acc := acc.insert p.fvarId entry
    acc ← match monoDecl.value with | .code c => collectStructTypes c acc | _ => pure acc
  return acc

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

    -- Pre-allocate slots and types for all join point parameters
    let joinPoints := collectJoinPoints code
    let mut joinPointMap : Std.HashMap FVarId (FunDecl .impure) := {}
    for jp in joinPoints do
      joinPointMap := joinPointMap.insert jp.fvarId jp
      for p in jp.params do
        slotMap := slotMap.insert p.fvarId slot
        typeMap := typeMap.insert p.fvarId p.type
        slot := slot + getSlotSize p.type

    -- Reset code, fixups, and labelOffsets buffer for this method
    modify fun s => { s with code := ByteArray.empty, fixups := #[], labelOffsets := {} }

    let ctorClassMap ← if isBoxed then pure {} else populateCtorClassMap decl
    let ctx ← read
    let methodCtx := { ctx with
      currFn := decl.name
      currReturnType := retType
      currParams := decl.params
      varSlotMap := slotMap
      varTypeMap := typeMap
      ctorClassMap := ctorClassMap
      nextSlot := slot
      joinPoints := joinPointMap
    }

    withReader (fun _ => methodCtx) do
      emitCode code

    let mut bytecodes := (← get).code
    for (fpc, targetName) in (← get).fixups do
      if targetName == `_start then
        let target := (0x10000 - fpc).toUInt16
        bytecodes := bytecodes.set! (fpc + 1) (target >>> 8).toUInt8
        bytecodes := bytecodes.set! (fpc + 2) target.toUInt8
      else if let some targetOffset := (← get).labelOffsets[targetName]? then
        let targetDiff : Int := (targetOffset : Int) - (fpc : Int)
        let targetNat := if targetDiff < 0 then (0x10000 + targetDiff).toNat else targetDiff.toNat
        let target := targetNat.toUInt16
        bytecodes := bytecodes.set! (fpc + 1) (target >>> 8).toUInt8
        bytecodes := bytecodes.set! (fpc + 2) target.toUInt8

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
    ctorClassMap := {}
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
