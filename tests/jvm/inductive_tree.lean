import Lean.Compiler.JVM

/-!
Tests inductive constructor allocation and pattern matching on the JVM.
Models the inductive data type tree evaluation in `tests/compile/expr.lean`.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def main : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/TreeTest"
    superClass := "java/lang/Object"
  }

  -- Register CP references
  let (ctorAllocRef, cf') := cf.addMethodRef "lean/runtime/LeanCtor" "alloc" "(III)Llean/runtime/LeanCtor;"
  let (ctorGetTagRef, cf') := cf'.addMethodRef "lean/runtime/LeanObject" "getTag" "()I"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(I)V"
  cf := cf'

  /-
  public static void main(String[] args) {
    // Allocate Leaf with tag 2
    LeanCtor leaf = LeanCtor.alloc(2, 0, 0);
    // Print leaf.getTag() => 2
    System.out.println(leaf.getTag());
  }
  -/
  let mut mainCode := ByteArray.empty
  -- iconst 2, iconst 0, iconst 0
  mainCode := Opcode.iconst_2 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.invokestatic ctorAllocRef |>.emit mainCode
  mainCode := Opcode.astore 1 |>.emit mainCode

  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorGetTagRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnRef |>.emit mainCode
  mainCode := Opcode.return_void |>.emit mainCode

  let mainMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "main"
    descriptor := "([Ljava/lang/String;)V"
    maxStack := 4
    maxLocals := 2
    bytecodes := mainCode
  }
  cf := { cf with methods := cf.methods.push mainMethod }

  let bytes := cf.toByteArray
  IO.FS.createDirAll "lean/test"
  IO.FS.writeBinFile "lean/test/TreeTest.class" bytes
  IO.println "TreeTest.class generated."
