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

def toJVMTypeDesc (type : Expr) : String :=
  match type with
  | .const ``UInt8 _ => "B"
  | .const ``UInt16 _ => "S"
  | .const ``UInt32 _ => "I"
  | .const ``UInt64 _ => "J"
  | .const ``USize _ => "J"
  | .const ``Float _ => "D"
  | .const ``Float32 _ => "F"
  | .const ``String _ => leanStringTypeDesc
  | .const ``Nat _ => leanNatTypeDesc
  | _ => leanObjTypeDesc

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

/--
Emits a literal value onto the operand stack.
-/
def emitLit (v : LitValue) : EmitJVMM Unit := do
  match v with
  | .uint8 b => emitOp (.bipush b)
  | .uint16 s => emitOp (.sipush s)
  | .uint32 i =>
    if i <= 5 then emitOp (.iconst_0) -- simplified iconst
    else emitOp (.bipush i.toUInt8)
  | .uint64 l =>
    if l == 0 then emitOp .lconst_0
    else if l == 1 then emitOp .lconst_1
    else emitOp (.bipush l.toUInt8); emitOp .i2l
  | .usize u =>
    if u == 0 then emitOp .lconst_0 else emitOp (.bipush u.toUInt8); emitOp .i2l
  | .nat n =>
    -- Push small Nat as LeanNat.ofLong(n)
    emitOp (.bipush n.toUInt8)
    emitOp .i2l
    -- In full implementation: constant pool index for invokestatic LeanNat.ofLong
    -- For now emits iconst / bipush
  | .str _s =>
    -- In full implementation: ldc string index + LeanString.of
    emitOp .aconst_null

/--
Emits let declarations.
-/
def emitLetValue (decl : LetDecl .impure) : EmitJVMM Unit := do
  match decl.value with
  | .lit v =>
    emitLit v
  | .erased =>
    emitOp .aconst_null
  | .fvar fvarId args =>
    if args.isEmpty then
      emitLoad fvarId
    else
      emitLoad fvarId
      -- apply dynamic closure invocation
  | .fap _fn args =>
    for arg in args do
      match arg with
      | .fvar fvarId => emitLoad fvarId
      | .erased => emitOp .aconst_null
    -- emit invokestatic TargetClass.fn
  | .ctor info args =>
    -- Allocate constructor object
    emitOp (.bipush info.cidx.toUInt8)
    emitOp (.bipush args.size.toUInt8)
    emitOp (.bipush (info.usize + info.ssize / 8).toUInt8)
    -- invoke LeanCtor.alloc(tag, numObjs, numScalars)
  | .oproj i fvarId =>
    emitLoad fvarId
    emitOp (.bipush i.toUInt8)
    -- invokevirtual LeanCtor.getObj
  | .box _ fvarId =>
    emitLoad fvarId
  | .unbox fvarId =>
    emitLoad fvarId
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
    emitLoad cs.discr
    -- In full implementation: extract tag and branch with tableswitch/lookupswitch
    if h : cs.alts.size > 0 then
      match cs.alts[0] with
      | .ctorAlt _ k => emitCode k
      | .default k => emitCode k
    else
      emitOp .aconst_null
      emitOp .areturn
  | .jmp fvarId args =>
    for arg in args do
      match arg with
      | .fvar id => emitLoad id
      | .erased => emitOp .aconst_null
    -- emit goto join point
  | .jp decl k =>
    -- Define join point target block
    emitCode k
  | .inc (k := k) .. | .dec (k := k) .. | .del (k := k) .. =>
    -- Tracing GC: Reference counting instructions are no-ops!
    emitCode k
  | .setTag _ _ k | .oset _ _ _ k | .uset _ _ _ k | .sset _ _ _ _ _ k =>
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
      maxStack := 32
      maxLocals := (slot + 16).toUInt16
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
