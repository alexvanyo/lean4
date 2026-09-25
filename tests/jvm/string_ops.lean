import Lean.Compiler.JVM

/-!
Tests UTF-8 string creation, byte extraction, length and concatenation on JVM.
Models string operations in `tests/compile/str.lean`.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def main : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/StringOpsTest"
    superClass := "java/lang/Object"
  }

  -- Register CP references
  let (strOfRef, cf') := cf.addMethodRef "lean/runtime/LeanString" "of" "(Ljava/lang/String;)Llean/runtime/LeanString;"
  let (strConcatRef, cf') := cf'.addMethodRef "lean/runtime/LeanString" "concat" "(Llean/runtime/LeanString;Llean/runtime/LeanString;)Llean/runtime/LeanString;"
  let (strByteSizeRef, cf') := cf'.addMethodRef "lean/runtime/LeanString" "getByteSize" "()I"
  let (strSubstringRef, cf') := cf'.addMethodRef "lean/runtime/LeanString" "substringByByte" "(II)Llean/runtime/LeanString;"
  let (strHelloRef, cf') := cf'.addString "hello "
  let (strWorldRef, cf') := cf'.addString "world"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnObjRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"
  let (printlnIntRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(I)V"
  cf := cf'

  /-
  public static void main(String[] args) {
    LeanString s1 = LeanString.of("hello ");
    LeanString s2 = LeanString.of("world");
    LeanString s3 = LeanString.concat(s1, s2);
    System.out.println(s3);
    System.out.println(s3.getByteSize());
    LeanString s4 = s3.substringByByte(0, 5);
    System.out.println(s4);
  }
  -/
  let mut mainCode := ByteArray.empty
  -- s1 = LeanString.of("hello ")
  mainCode := Opcode.ldc (strHelloRef.toUInt8) |>.emit mainCode
  mainCode := Opcode.invokestatic strOfRef |>.emit mainCode
  mainCode := Opcode.astore 1 |>.emit mainCode

  -- s2 = LeanString.of("world")
  mainCode := Opcode.ldc (strWorldRef.toUInt8) |>.emit mainCode
  mainCode := Opcode.invokestatic strOfRef |>.emit mainCode
  mainCode := Opcode.astore 2 |>.emit mainCode

  -- s3 = LeanString.concat(s1, s2)
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.aload 2 |>.emit mainCode
  mainCode := Opcode.invokestatic strConcatRef |>.emit mainCode
  mainCode := Opcode.astore 3 |>.emit mainCode

  -- System.out.println(s3)
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 3 |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode

  -- System.out.println(s3.getByteSize())
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 3 |>.emit mainCode
  mainCode := Opcode.invokevirtual strByteSizeRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnIntRef |>.emit mainCode

  -- s4 = s3.substringByByte(0, 5)
  mainCode := Opcode.aload 3 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.iconst_5 |>.emit mainCode
  mainCode := Opcode.invokevirtual strSubstringRef |>.emit mainCode
  mainCode := Opcode.astore 4 |>.emit mainCode

  -- System.out.println(s4)
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 4 |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode
  mainCode := Opcode.return_void |>.emit mainCode

  let mainMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "main"
    descriptor := "([Ljava/lang/String;)V"
    maxStack := 4
    maxLocals := 5
    bytecodes := mainCode
  }
  cf := { cf with methods := cf.methods.push mainMethod }

  let bytes := cf.toByteArray
  IO.FS.createDirAll "lean/test"
  IO.FS.writeBinFile "lean/test/StringOpsTest.class" bytes
  IO.println "StringOpsTest.class generated."
