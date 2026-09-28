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

def toJVMTypeDesc (_type : Expr) : String :=
  leanObjTypeDesc

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
  currParams       : Array (Param .impure) := #[]
  varSlotMap       : Std.HashMap FVarId UInt8 := {}
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

def allocSlot (fvarId : FVarId) : EmitJVMM (UInt8 × JVMContext) := do
  let ctx ← read
  let slot := ctx.nextSlot
  let newMap := ctx.varSlotMap.insert fvarId slot
  let newCtx := { ctx with varSlotMap := newMap, nextSlot := slot + 1 }
  return (slot, newCtx)

def emitLoad (fvarId : FVarId) : EmitJVMM Unit := do
  let slot ← getSlot fvarId
  emitOp (.aload slot)

def emitStore (fvarId : FVarId) : EmitJVMM Unit := do
  let slot ← getSlot fvarId
  emitOp (.astore slot)

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
    | .fvar fvarId => emitLoad fvarId
    | .erased => emitOp .aconst_null
  -- Call <init>(cap0..capN)
  let mut initParamDesc := ""
  for _ in List.range numCaptured do
    initParamDesc := initParamDesc ++ leanObjTypeDesc
  let initDesc := s!"({initParamDesc})V"
  let initRef ← addMethodRef synClassName "<init>" initDesc
  emitOp (.invokespecial initRef)
  return synClassName


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
  else
    emitOp (.sipush v.toUInt16)

def addString (str : String) : EmitJVMM UInt16 := do
  let s ← get
  let (idx, cf') := s.cf.addString str
  set { s with cf := cf' }
  return idx

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
  let numScalars :=
    if info.usize == 0 && info.ssize == 0 then 0
    else if info.ssize > info.usize + (info.ssize + 7) / 8 then info.ssize
    else info.usize + (info.ssize + 7) / 8
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
      | .fvar fvarId => emitLoad fvarId
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
    | .uint8 b => emitPushNatLiteral b.toNat
    | .uint16 s => emitPushNatLiteral s.toNat
    | .uint32 i => emitPushNatLiteral i.toNat
    | .uint64 l => emitPushNatLiteral l.toNat
    | .usize u => emitPushNatLiteral u.toNat
  | .erased =>
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
        | .fvar argId => emitLoad argId
        | .erased => emitOp .aconst_null
        let applyIdx ← addMethodRef "lean/runtime/LeanClosure" "apply1" "(Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
        emitOp (.invokevirtual applyIdx)
      else if args.size == 2 then
        match args[0]! with
        | .fvar argId => emitLoad argId
        | .erased => emitOp .aconst_null
        match args[1]! with
        | .fvar argId => emitLoad argId
        | .erased => emitOp .aconst_null
        let applyIdx ← addMethodRef "lean/runtime/LeanClosure" "apply2" "(Llean/runtime/LeanObject;Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
        emitOp (.invokevirtual applyIdx)
      else if args.size == 3 then
        match args[0]! with
        | .fvar argId => emitLoad argId
        | .erased => emitOp .aconst_null
        match args[1]! with
        | .fvar argId => emitLoad argId
        | .erased => emitOp .aconst_null
        match args[2]! with
        | .fvar argId => emitLoad argId
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
          | .fvar argId => emitLoad argId
          | .erased => emitOp .aconst_null
          emitOp .aastore
        let applyIdx ← addMethodRef "lean/runtime/LeanClosure" "apply" "([Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
        emitOp (.invokevirtual applyIdx)
  | .fap fn args =>
    let targetClass ← getDeclClassName fn
    let methodName := toJVMMethodName fn
    let sig? ← getImpureSignature? fn
    let (paramDescs, retDesc) := match sig? with
      | some sig => Id.run do
        let mut pDescs := ""
        for p in sig.params do
          pDescs := pDescs ++ toJVMTypeDesc p.type
        return (pDescs, toJVMTypeDesc sig.type)
      | none => Id.run do
        let mut pDescs := ""
        for _ in args do
          pDescs := pDescs ++ leanObjTypeDesc
        return (pDescs, leanObjTypeDesc)
    let descriptor := s!"({paramDescs}){retDesc}"
    for arg in args do
      match arg with
      | .fvar fvarId => emitLoad fvarId
      | .erased => emitOp .aconst_null
    let methodRef ← addMethodRef targetClass methodName descriptor
    emitOp (.invokestatic methodRef)
  | .pap fn args =>
    let targetClass ← getDeclClassName fn
    let methodName := toJVMMethodName fn
    let totalArity? := match ← getImpureSignature? fn with
      | some sig => some sig.params.size
      | none => none
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
          | .fvar argId => emitLoad argId
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
        | .fvar argId => emitLoad argId
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
    let ofLongIdx ← addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
    emitOp (.invokestatic ofLongIdx)
  | .sproj _n offset fvarId =>
    emitLoad fvarId
    let ctorClassIdx ← addClass "lean/runtime/LeanCtor"
    emitOp (.checkcast ctorClassIdx)
    let scalarIdx := if offset >= 8 then offset / 8 else offset
    emitPushInt scalarIdx
    let getScalarIdx ← addMethodRef "lean/runtime/LeanCtor" "getScalar" "(I)J"
    emitOp (.invokevirtual getScalarIdx)
    let ofLongIdx ← addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
    emitOp (.invokestatic ofLongIdx)
  | .box _ fvarId =>
    emitLoad fvarId
  | .unbox fvarId =>
    emitLoad fvarId
  | .reuse _ info _ args =>
    emitCtor info args
  | .reset .. =>
    emitOp .aconst_null
  | .isShared _fvarId =>
    emitOp .iconst_1
    emitOp .i2l
    let ofLongIdx ← addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
    emitOp (.invokestatic ofLongIdx)
  | _ =>
    emitOp .aconst_null

/--
Emits a basic block of LCNF code to JVM bytecode.
-/
partial def emitCode (code : Code .impure) : EmitJVMM Unit := do
  match code with
  | .let decl k =>
    emitLetValue decl
    let (slot, newCtx) ← allocSlot decl.fvarId
    emitOp (.astore slot)
    withReader (fun _ => newCtx) do
      emitCode k
  | .return fvarId =>
    emitLoad fvarId
    emitOp .areturn
  | .unreach .. =>
    emitOp .aconst_null
    emitOp .areturn
  | .cases cs =>
    if cs.alts.isEmpty then
      emitOp .aconst_null
      emitOp .areturn
    else if cs.alts.size == 1 then
      match cs.alts[0]! with
      | .ctorAlt _ k => emitCode k
      | .default k => emitCode k
    else
      let ctx ← read
      let tagSlot := ctx.nextSlot
      let branchCtx := { ctx with nextSlot := ctx.nextSlot + 1 }
      emitLoad cs.discr
      let getTagIdx ← addMethodRef "lean/runtime/LeanObject" "getTag" "()I"
      emitOp (.invokevirtual getTagIdx)
      emitOp (.istore tagSlot)
      withReader (fun _ => branchCtx) do
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
        emitOp .aconst_null
        emitOp .areturn
  | .jmp fvarId args =>
    let ctx ← read
    match ctx.joinPoints[fvarId]? with
    | some decl =>
      let mut nextSlot := ctx.nextSlot
      let mut newVarSlotMap := ctx.varSlotMap
      let mut paramSlots : Array (Param .impure × UInt8) := #[]
      for p in decl.params do
        paramSlots := paramSlots.push (p, nextSlot)
        newVarSlotMap := newVarSlotMap.insert p.fvarId nextSlot
        nextSlot := nextSlot + 1

      for h : i in 0...args.size do
        let arg := args[i]
        if h2 : i < paramSlots.size then
          let (_, slot) := paramSlots[i]
          match arg with
          | .fvar id => emitLoad id
          | .erased => emitOp .aconst_null
          emitOp (.astore slot)
      let newCtx := { ctx with varSlotMap := newVarSlotMap, nextSlot := nextSlot }
      withReader (fun _ => newCtx) do
        emitCode decl.value
    | none =>
      emitOp .aconst_null
      emitOp .areturn
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
    | .fvar yId => emitLoad yId
    | .erased => emitOp .aconst_null
    let setObjIdx ← addMethodRef "lean/runtime/LeanCtor" "setObj" "(ILlean/runtime/LeanObject;)V"
    emitOp (.invokevirtual setObjIdx)
    emitCode k
  | .uset fvarId i y k =>
    emitLoad fvarId
    let ctorClassIdx ← addClass "lean/runtime/LeanCtor"
    emitOp (.checkcast ctorClassIdx)
    emitPushInt i
    emitLoad y
    let getScalar64Idx ← addMethodRef "lean/runtime/LeanRuntimeJVM" "getScalar64" "(Llean/runtime/LeanObject;)J"
    emitOp (.invokestatic getScalar64Idx)
    let setScalarIdx ← addMethodRef "lean/runtime/LeanCtor" "setScalar" "(IJ)V"
    emitOp (.invokevirtual setScalarIdx)
    emitCode k
  | .sset fvarId _i offset y _ty k =>
    emitLoad fvarId
    let ctorClassIdx ← addClass "lean/runtime/LeanCtor"
    emitOp (.checkcast ctorClassIdx)
    let scalarIdx := if offset >= 8 then offset / 8 else offset
    emitPushInt scalarIdx
    emitLoad y
    let getScalar64Idx ← addMethodRef "lean/runtime/LeanRuntimeJVM" "getScalar64" "(Llean/runtime/LeanObject;)J"
    emitOp (.invokestatic getScalar64Idx)
    let setScalarIdx ← addMethodRef "lean/runtime/LeanCtor" "setScalar" "(IJ)V"
    emitOp (.invokevirtual setScalarIdx)
    emitCode k
  | .setTag _ _ k =>
    emitCode k

/--
Emits a function declaration as a static method in the classfile.
-/
def emitFnDecl (decl : Decl .impure) : EmitJVMM Unit := do
  match decl.value with
  | .extern .. => return ()
  | .code code =>
    let methodName := toJVMMethodName decl.name
    let mut paramDescs := ""
    let mut slot : UInt8 := 0
    let mut slotMap : Std.HashMap FVarId UInt8 := {}
    for p in decl.params do
      slotMap := slotMap.insert p.fvarId slot
      paramDescs := paramDescs ++ toJVMTypeDesc p.type
      slot := slot + 1

    let retDesc := toJVMTypeDesc decl.type
    let descriptor := s!"({paramDescs}){retDesc}"

    -- Reset code buffer for this method
    modify fun s => { s with code := ByteArray.empty }

    let ctx ← read
    let methodCtx := { ctx with
      currFn := decl.name
      currParams := decl.params
      varSlotMap := slotMap
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
Emits a public static void main entry point if the module contains a `main` declaration.
-/
def emitMainIfNeeded : EmitJVMM Unit := do
  let hasMain := (← read).localDecls.any (·.name == `main)
  if hasMain then
    let mainMethod : MethodDef := {
      accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
      name := "main"
      descriptor := "([Ljava/lang/String;)V"
      maxStack := 4
      maxLocals := 2
      -- invokes module's _lean_main and exits
      bytecodes := (Opcode.return_void.emit ByteArray.empty)
    }
    modify fun s => { s with cf := { s.cf with methods := s.cf.methods.push mainMethod } }

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
    emitMainIfNeeded
  ).run ctx |>.run { cf := initialCF } |>.run (phase := .impure)

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
