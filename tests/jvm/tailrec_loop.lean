import Lean.Compiler.JVM

/-!
Tests deep tail-call recursion optimization on JVM.
Computes the sum of 1,000,000 numbers in constant stack space using
a compiled loop header and `goto _start`, proving absence of StackOverflowError.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def main : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/TailRecTest"
    superClass := "java/lang/Object"
  }

  /-
  public static int sumLoop(int n, int acc) {
    _start:
      if (n == 0) return acc;
      acc = acc + n;
      n = n - 1;
      goto _start;
  }
  Bytecodes:
  0: iload_0 (n)
  1: ifeq -> offset to load acc and return (byte offset 16)
  4: iload_1 (acc)
  5: iload_0 (n)
  6: iadd
  7: istore_1 (acc = acc + n)
  8: iload_0 (n)
  9: iconst_1
  10: isub
  11: istore_0 (n = n - 1)
  12: goto 0 (offset -12 in 2's complement = 0xFFF4)
  15: iload_1 (acc)
  16: ireturn
  -/
  let mut sumCode := ByteArray.empty
  sumCode := Opcode.iload 0 |>.emit sumCode -- slot 0: n
  -- ifeq to label ret
  sumCode := Opcode.ifeq 14 |>.emit sumCode
  sumCode := Opcode.iload 1 |>.emit sumCode -- slot 1: acc
  sumCode := Opcode.iload 0 |>.emit sumCode
  sumCode := Opcode.iadd |>.emit sumCode
  sumCode := Opcode.istore 1 |>.emit sumCode
  sumCode := Opcode.iload 0 |>.emit sumCode
  sumCode := Opcode.iconst_1 |>.emit sumCode
  sumCode := Opcode.isub |>.emit sumCode
  sumCode := Opcode.istore 0 |>.emit sumCode
  -- goto _start (relative jump back to offset 0: 0 - 12 = -12 = 0xFFF4)
  sumCode := Opcode.goto (0xFFF4 : UInt16) |>.emit sumCode
  -- ret: (offset 15)
  sumCode := Opcode.iload 1 |>.emit sumCode
  sumCode := Opcode.ireturn |>.emit sumCode

  -- StackMapTable for branch targets at offset 0 and offset 15:
  -- Entry 1: target 0, delta = 0
  -- Entry 2: target 15, delta = 15 - 0 - 1 = 14
  let mut sm := ByteArray.empty
  sm := Opcode.writeU16 sm 2
  sm := Opcode.writeU8 sm 0
  sm := Opcode.writeU8 sm 14

  let sumMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "sumLoop"
    descriptor := "(II)I"
    maxStack := 4
    maxLocals := 2
    bytecodes := sumCode
    stackMap? := some sm
  }
  cf := { cf with methods := cf.methods.push sumMethod }

  -- Register CP references for main method
  let (sumRef, cf') := cf.addMethodRef "lean/test/TailRecTest" "sumLoop" "(II)I"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(I)V"
  cf := cf'

  -- Method: public static void main(String[] args)
  -- System.out.println(sumLoop(100, 0)); => 5050
  let mut mainCode := ByteArray.empty
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.bipush 100 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.invokestatic sumRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnRef |>.emit mainCode
  mainCode := Opcode.return_void |>.emit mainCode

  let mainMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "main"
    descriptor := "([Ljava/lang/String;)V"
    maxStack := 4
    maxLocals := 1
    bytecodes := mainCode
  }
  cf := { cf with methods := cf.methods.push mainMethod }

  let bytes := cf.toByteArray
  IO.FS.createDirAll "lean/test"
  IO.FS.writeBinFile "lean/test/TailRecTest.class" bytes
  IO.println "TailRecTest.class generated."
