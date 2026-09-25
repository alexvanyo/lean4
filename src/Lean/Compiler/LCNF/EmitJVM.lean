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

/--
Emits let declarations to JVM bytecode.
-/
def emitLetValue (decl : LetDecl .impure) : EmitJVMM Unit := do
  match decl.value with
  | .lit v =>
    match v with
    | .nat n =>
      if n <= 127 then
        emitOp (.bipush n.toUInt8)
        emitOp .i2l
      else
        emitOp (.sipush n.toUInt16)
        emitOp .i2l
      let ofLongIdx ← addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
      emitOp (.invokestatic ofLongIdx)
    | .str s =>
      let strIdx ← addString s
      if strIdx <= 255 then
        emitOp (.ldc strIdx.toUInt8)
      else
        emitOp (.ldc_w strIdx)
      let ofStrIdx ← addMethodRef "lean/runtime/LeanString" "of" "(Ljava/lang/String;)Llean/runtime/LeanString;"
      emitOp (.invokestatic ofStrIdx)
    | .uint8 b =>
      emitOp (.bipush b)
      emitOp .i2l
      let ofLongIdx ← addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
      emitOp (.invokestatic ofLongIdx)
    | .uint16 s =>
      emitOp (.sipush s)
      emitOp .i2l
      let ofLongIdx ← addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
      emitOp (.invokestatic ofLongIdx)
    | .uint32 i =>
      if i <= 127 then emitOp (.bipush i.toUInt8) else emitOp (.sipush i.toUInt16)
      emitOp .i2l
      let ofLongIdx ← addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
      emitOp (.invokestatic ofLongIdx)
    | .uint64 l =>
      if l == 0 then emitOp .lconst_0
      else if l == 1 then emitOp .lconst_1
      else emitOp (.bipush l.toUInt8); emitOp .i2l
      let ofLongIdx ← addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
      emitOp (.invokestatic ofLongIdx)
    | .usize u =>
      if u == 0 then emitOp .lconst_0
      else emitOp (.bipush u.toUInt8); emitOp .i2l
      let ofLongIdx ← addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
      emitOp (.invokestatic ofLongIdx)
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
    let arity := match ← getImpureSignature? fn with
      | some sig => sig.params.size
      | none => args.size
    let classStrRef ← addString targetClass
    if classStrRef <= 255 then
      emitOp (.ldc classStrRef.toUInt8)
    else
      emitOp (.ldc_w classStrRef)
    let methodStrRef ← addString methodName
    if methodStrRef <= 255 then
      emitOp (.ldc methodStrRef.toUInt8)
    else
      emitOp (.ldc_w methodStrRef)
    emitPushInt arity
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
    let allocClosureIdx ← addMethodRef "lean/runtime/LeanClosure" "alloc" "(Ljava/lang/String;Ljava/lang/String;I[Llean/runtime/LeanObject;)Llean/runtime/LeanClosure;"
    emitOp (.invokestatic allocClosureIdx)
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
    let getTagIdx ← addMethodRef "lean/runtime/LeanObject" "getTag" "()I"
    emitOp (.invokevirtual getTagIdx)
    emitOp .i2l
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
    let getTagIdx ← addMethodRef "lean/runtime/LeanObject" "getTag" "()I"
    emitOp (.invokevirtual getTagIdx)
    emitOp .i2l
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
-/
public def emitJVMForDecls (modName : Name) (decls : Array Name) : CoreM ByteArray := do
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

  return s.cf.toByteArray

/--
Emits JVM bytecode (.class file) for the given module.
-/
public def emitJVM (modName : Name) : CoreM ByteArray := do
  emitJVMForDecls modName (← getLocalImpureDecls)

end JVM

end

end Lean.Compiler.LCNF
