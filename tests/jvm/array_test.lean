import Lean.Compiler.JVM

/-!
Tests array creation, push, size and element access on JVM.
Models array manipulation in `tests/compile/array_test.lean`.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def main : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/ArrayTest"
    superClass := "java/lang/Object"
  }

  -- Register CP references
  let (arrayEmptyRef, cf') := cf.addMethodRef "lean/runtime/LeanArray" "empty" "()Llean/runtime/LeanArray;"
  let (arrayPushRef, cf') := cf'.addMethodRef "lean/runtime/LeanArray" "push" "(Llean/runtime/LeanObject;)Llean/runtime/LeanArray;"
  let (arraySizeRef, cf') := cf'.addMethodRef "lean/runtime/LeanArray" "size" "()I"
  let (arrayGetRef, cf') := cf'.addMethodRef "lean/runtime/LeanArray" "get" "(I)Llean/runtime/LeanObject;"
  let (natOfLongRef, cf') := cf'.addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnObjRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"
  let (printlnIntRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(I)V"
  cf := cf'

  /-
  public static void main(String[] args) {
    LeanArray a = LeanArray.empty();
    a = a.push(LeanNat.ofLong(0));
    a = a.push(LeanNat.ofLong(1));
    a = a.push(LeanNat.ofLong(2));
    a = a.push(LeanNat.ofLong(3));
    System.out.println(a);        // #[0, 1, 2, 3]
    System.out.println(a.size()); // 4
    System.out.println(a.get(2)); // 2
  }
  -/
  let mut mainCode := ByteArray.empty
  -- a = LeanArray.empty()
  mainCode := Opcode.invokestatic arrayEmptyRef |>.emit mainCode
  mainCode := Opcode.astore 1 |>.emit mainCode -- local 1 = a

  -- a = a.push(LeanNat.ofLong(0))
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.lconst_0 |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokevirtual arrayPushRef |>.emit mainCode
  mainCode := Opcode.astore 1 |>.emit mainCode

  -- a = a.push(LeanNat.ofLong(1))
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.lconst_1 |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokevirtual arrayPushRef |>.emit mainCode
  mainCode := Opcode.astore 1 |>.emit mainCode

  -- a = a.push(LeanNat.ofLong(2))
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.bipush 2 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokevirtual arrayPushRef |>.emit mainCode
  mainCode := Opcode.astore 1 |>.emit mainCode

  -- a = a.push(LeanNat.ofLong(3))
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.bipush 3 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokevirtual arrayPushRef |>.emit mainCode
  mainCode := Opcode.astore 1 |>.emit mainCode

  -- System.out.println(a)
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode

  -- System.out.println(a.size())
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.invokevirtual arraySizeRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnIntRef |>.emit mainCode

  -- System.out.println(a.get(2))
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.iconst_2 |>.emit mainCode
  mainCode := Opcode.invokevirtual arrayGetRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode

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
  IO.FS.writeBinFile "lean/test/ArrayTest.class" bytes
  IO.println "ArrayTest.class generated."
