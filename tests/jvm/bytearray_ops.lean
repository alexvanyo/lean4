import Lean.Compiler.JVM

/-!
Tests native ByteArray allocation, append, and byte indexing on JVM.
Models byte array operations in `tests/compile/bytearray_bug.lean`.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def main : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/ByteArrayOpsTest"
    superClass := "java/lang/Object"
  }

  let (emptyRef, cf') := cf.addMethodRef "lean/runtime/LeanByteArray" "empty" "()Llean/runtime/LeanByteArray;"
  let (pushRef, cf') := cf'.addMethodRef "lean/runtime/LeanByteArray" "push" "(B)Llean/runtime/LeanByteArray;"
  let (sizeRef, cf') := cf'.addMethodRef "lean/runtime/LeanByteArray" "size" "()I"
  let (getRef, cf') := cf'.addMethodRef "lean/runtime/LeanByteArray" "get" "(I)B"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnIntRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(I)V"
  cf := cf'

  /-
  public static void main(String[] args) {
    LeanByteArray arr = LeanByteArray.empty();
    arr = arr.push((byte) 10);
    arr = arr.push((byte) 42);
    System.out.println(arr.size()); // 2
    System.out.println(arr.get(0)); // 10
    System.out.println(arr.get(1)); // 42
  }
  -/
  let mut mainCode := ByteArray.empty
  -- arr = LeanByteArray.empty()
  mainCode := Opcode.invokestatic emptyRef |>.emit mainCode
  mainCode := Opcode.astore 1 |>.emit mainCode -- local 1 = arr

  -- arr = arr.push((byte) 10)
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.bipush 10 |>.emit mainCode
  mainCode := Opcode.invokevirtual pushRef |>.emit mainCode
  mainCode := Opcode.astore 1 |>.emit mainCode

  -- arr = arr.push((byte) 42)
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.bipush 42 |>.emit mainCode
  mainCode := Opcode.invokevirtual pushRef |>.emit mainCode
  mainCode := Opcode.astore 1 |>.emit mainCode

  -- System.out.println(arr.size())
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.invokevirtual sizeRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnIntRef |>.emit mainCode

  -- System.out.println(arr.get(0))
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.invokevirtual getRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnIntRef |>.emit mainCode

  -- System.out.println(arr.get(1))
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.invokevirtual getRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnIntRef |>.emit mainCode
  mainCode := Opcode.return_void |>.emit mainCode

  let mainMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "main"
    descriptor := "([Ljava/lang/String;)V"
    maxStack := 3
    maxLocals := 2
    bytecodes := mainCode
  }
  cf := { cf with methods := cf.methods.push mainMethod }

  IO.FS.createDirAll "lean/test"
  IO.FS.writeBinFile "lean/test/ByteArrayOpsTest.class" cf.toByteArray
  IO.println "ByteArrayOpsTest.class generated."
