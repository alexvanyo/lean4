import Lean.Compiler.JVM

/-!
Tests unboxed integer arithmetic, recursion, and 8-bit overflow truncation on JVM.
Models scalar numerical operations in `tests/compile/uint_fold.lean`.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def main : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/UIntFoldTest"
    superClass := "java/lang/Object"
  }

  let (sysOutRef, cf') := cf.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnIntRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(I)V"
  cf := cf'

  /-
  public static int f(int x, int y) {
    int a1 = 12700;
    int v = 10 + x;
    int a2 = x + a1;
    return y + a2 + v + 10;
  }
  -/
  let mut fCode := ByteArray.empty
  fCode := Opcode.iload 1 |>.emit fCode -- y
  fCode := Opcode.iload 0 |>.emit fCode -- x
  -- push 12700
  let (c12700Ref, cf') := cf.addInteger 12700
  cf := cf'
  fCode := Opcode.ldc c12700Ref.toUInt8 |>.emit fCode
  fCode := Opcode.iadd |>.emit fCode -- x + a1
  fCode := Opcode.iadd |>.emit fCode -- y + a2
  fCode := Opcode.bipush 10 |>.emit fCode
  fCode := Opcode.iload 0 |>.emit fCode
  fCode := Opcode.iadd |>.emit fCode -- 10 + x
  fCode := Opcode.iadd |>.emit fCode
  fCode := Opcode.bipush 10 |>.emit fCode
  fCode := Opcode.iadd |>.emit fCode
  fCode := Opcode.ireturn |>.emit fCode

  let fMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "f"
    descriptor := "(II)I"
    maxStack := 4
    maxLocals := 2
    bytecodes := fCode
  }
  cf := { cf with methods := cf.methods.push fMethod }

  /-
  public static int g(int x, int y) {
    _start:
      if (x == 0) return y;
      y = y + 2;
      x = x - 1;
      goto _start;
  }
  Bytecodes:
  0: iload_0
  1: ifeq -> offset 14 (ireturn)
  4: iload_1
  5: iconst_2
  6: iadd
  7: istore_1
  8: iload_0
  9: iconst_1
  10: isub
  11: istore_0
  12: goto -12 (0xFFF4)
  15: iload_1
  16: ireturn
  -/
  let mut gCode := ByteArray.empty
  gCode := Opcode.iload 0 |>.emit gCode
  gCode := Opcode.ifeq 14 |>.emit gCode
  gCode := Opcode.iload 1 |>.emit gCode
  gCode := Opcode.iconst_2 |>.emit gCode
  gCode := Opcode.iadd |>.emit gCode
  gCode := Opcode.istore 1 |>.emit gCode
  gCode := Opcode.iload 0 |>.emit gCode
  gCode := Opcode.iconst_1 |>.emit gCode
  gCode := Opcode.isub |>.emit gCode
  gCode := Opcode.istore 0 |>.emit gCode
  gCode := Opcode.goto (0xFFF4 : UInt16) |>.emit gCode
  gCode := Opcode.iload 1 |>.emit gCode
  gCode := Opcode.ireturn |>.emit gCode

  let gMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "g"
    descriptor := "(II)I"
    maxStack := 3
    maxLocals := 2
    bytecodes := gCode
  }
  cf := { cf with methods := cf.methods.push gMethod }

  /-
  public static int foo() {
    int x = 100;
    return (x + x + x) & 0xFF; // 44
  }
  -/
  let mut fooCode := ByteArray.empty
  fooCode := Opcode.bipush 100 |>.emit fooCode
  fooCode := Opcode.bipush 100 |>.emit fooCode
  fooCode := Opcode.iadd |>.emit fooCode
  fooCode := Opcode.bipush 100 |>.emit fooCode
  fooCode := Opcode.iadd |>.emit fooCode
  fooCode := Opcode.sipush 0xFF |>.emit fooCode
  fooCode := Opcode.iand |>.emit fooCode
  fooCode := Opcode.ireturn |>.emit fooCode

  let fooMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "foo"
    descriptor := "()I"
    maxStack := 3
    maxLocals := 0
    bytecodes := fooCode
  }
  cf := { cf with methods := cf.methods.push fooMethod }

  let (fRef, cf') := cf.addMethodRef "lean/test/UIntFoldTest" "f" "(II)I"
  let (gRef, cf') := cf'.addMethodRef "lean/test/UIntFoldTest" "g" "(II)I"
  let (fooRef, cf') := cf'.addMethodRef "lean/test/UIntFoldTest" "foo" "()I"
  cf := cf'

  /-
  public static void main(String[] args) {
    System.out.println(f(10, 20)); // 12760
    System.out.println(f(0, 0));   // 12720
    System.out.println(g(3, 5));   // 11
    System.out.println(g(0, 6));   // 6
    System.out.println(foo());      // 44
  }
  -/
  let mut mainCode := ByteArray.empty
  -- f(10, 20)
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.bipush 10 |>.emit mainCode
  mainCode := Opcode.bipush 20 |>.emit mainCode
  mainCode := Opcode.invokestatic fRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnIntRef |>.emit mainCode

  -- f(0, 0)
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.invokestatic fRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnIntRef |>.emit mainCode

  -- g(3, 5)
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.iconst_3 |>.emit mainCode
  mainCode := Opcode.iconst_5 |>.emit mainCode
  mainCode := Opcode.invokestatic gRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnIntRef |>.emit mainCode

  -- g(0, 6)
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.bipush 6 |>.emit mainCode
  mainCode := Opcode.invokestatic gRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnIntRef |>.emit mainCode

  -- foo()
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.invokestatic fooRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnIntRef |>.emit mainCode
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

  IO.FS.createDirAll "lean/test"
  IO.FS.writeBinFile "lean/test/UIntFoldTest.class" cf.toByteArray
  IO.println "UIntFoldTest.class generated."
