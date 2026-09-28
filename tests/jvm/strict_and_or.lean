import Lean.Compiler.JVM

/-!
Tests boolean constructors and conditional branching on JVM.
Models boolean operations and branch evaluation in `tests/compile/strictAndOr.lean`.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def main : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/StrictAndOrTest"
    superClass := "java/lang/Object"
  }

  -- Register CP references
  let (sysOutRef, cf') := cf.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnStrRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/String;)V"
  let (strTrueRef, cf') := cf'.addString "true"
  let (strFalseRef, cf') := cf'.addString "false"
  cf := cf'

  /-
  public static int strictOr(int a, int b) {
    if (a != 0) return 1;
    return b != 0 ? 1 : 0;
  }
  Bytecodes:
  0: iload_0
  1: ifne 8 -> offset to ret1
  4: iload_1
  5: ifne 4 -> offset to ret1
  8: iconst_0
  9: ireturn
  10: iconst_1
  11: ireturn
  -/
  let mut orCode := ByteArray.empty
  orCode := Opcode.iload 0 |>.emit orCode
  orCode := Opcode.ifne 9 |>.emit orCode -- jump to 10
  orCode := Opcode.iload 1 |>.emit orCode
  orCode := Opcode.ifne 5 |>.emit orCode -- jump to 10
  orCode := Opcode.iconst_0 |>.emit orCode
  orCode := Opcode.ireturn |>.emit orCode
  -- offset 10:
  orCode := Opcode.iconst_1 |>.emit orCode
  orCode := Opcode.ireturn |>.emit orCode

  let orMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "strictOr"
    descriptor := "(II)I"
    maxStack := 2
    maxLocals := 2
    bytecodes := orCode
  }
  cf := { cf with methods := cf.methods.push orMethod }

  /-
  public static int strictAnd(int a, int b) {
    if (a == 0) return 0;
    return b != 0 ? 1 : 0;
  }
  Bytecodes:
  0: iload_0
  1: ifeq 8 -> offset to ret0
  4: iload_1
  5: ifeq 4 -> offset to ret0
  8: iconst_1
  9: ireturn
  10: iconst_0
  11: ireturn
  -/
  let mut andCode := ByteArray.empty
  andCode := Opcode.iload 0 |>.emit andCode
  andCode := Opcode.ifeq 9 |>.emit andCode -- jump to 10
  andCode := Opcode.iload 1 |>.emit andCode
  andCode := Opcode.ifeq 5 |>.emit andCode -- jump to 10
  andCode := Opcode.iconst_1 |>.emit andCode
  andCode := Opcode.ireturn |>.emit andCode
  -- offset 10:
  andCode := Opcode.iconst_0 |>.emit andCode
  andCode := Opcode.ireturn |>.emit andCode

  let andMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "strictAnd"
    descriptor := "(II)I"
    maxStack := 2
    maxLocals := 2
    bytecodes := andCode
  }
  cf := { cf with methods := cf.methods.push andMethod }

  /-
  public static void printBool(int v) {
    System.out.println(v != 0 ? "true" : "false");
  }
  -/
  let mut pbCode := ByteArray.empty
  pbCode := Opcode.iload 0 |>.emit pbCode
  pbCode := Opcode.ifeq 12 |>.emit pbCode
  pbCode := Opcode.getstatic sysOutRef |>.emit pbCode
  pbCode := Opcode.ldc (strTrueRef.toUInt8) |>.emit pbCode
  pbCode := Opcode.invokevirtual printlnStrRef |>.emit pbCode
  pbCode := Opcode.return_void |>.emit pbCode
  pbCode := Opcode.getstatic sysOutRef |>.emit pbCode
  pbCode := Opcode.ldc (strFalseRef.toUInt8) |>.emit pbCode
  pbCode := Opcode.invokevirtual printlnStrRef |>.emit pbCode
  pbCode := Opcode.return_void |>.emit pbCode

  let pbMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "printBool"
    descriptor := "(I)V"
    maxStack := 3
    maxLocals := 1
    bytecodes := pbCode
  }
  cf := { cf with methods := cf.methods.push pbMethod }

  let (strictOrRef, cf') := cf.addMethodRef "lean/test/StrictAndOrTest" "strictOr" "(II)I"
  let (strictAndRef, cf') := cf'.addMethodRef "lean/test/StrictAndOrTest" "strictAnd" "(II)I"
  let (printBoolRef, cf') := cf'.addMethodRef "lean/test/StrictAndOrTest" "printBool" "(I)V"
  cf := cf'

  /-
  public static void main(String[] args) {
    printBool(strictOr(0, 0));
    printBool(strictOr(0, 1));
    printBool(strictOr(1, 0));
    printBool(strictOr(1, 1));
    printBool(strictAnd(0, 0));
    printBool(strictAnd(0, 1));
    printBool(strictAnd(1, 0));
    printBool(strictAnd(1, 1));
  }
  -/
  let mut mainCode := ByteArray.empty
  let pairs := #[(0, 0), (0, 1), (1, 0), (1, 1)]
  for (a, b) in pairs do
    if a == 0 then mainCode := Opcode.iconst_0 |>.emit mainCode
    else mainCode := Opcode.iconst_1 |>.emit mainCode
    if b == 0 then mainCode := Opcode.iconst_0 |>.emit mainCode
    else mainCode := Opcode.iconst_1 |>.emit mainCode
    mainCode := Opcode.invokestatic strictOrRef |>.emit mainCode
    mainCode := Opcode.invokestatic printBoolRef |>.emit mainCode

  for (a, b) in pairs do
    if a == 0 then mainCode := Opcode.iconst_0 |>.emit mainCode
    else mainCode := Opcode.iconst_1 |>.emit mainCode
    if b == 0 then mainCode := Opcode.iconst_0 |>.emit mainCode
    else mainCode := Opcode.iconst_1 |>.emit mainCode
    mainCode := Opcode.invokestatic strictAndRef |>.emit mainCode
    mainCode := Opcode.invokestatic printBoolRef |>.emit mainCode

  mainCode := Opcode.return_void |>.emit mainCode

  let mainMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "main"
    descriptor := "([Ljava/lang/String;)V"
    maxStack := 2
    maxLocals := 1
    bytecodes := mainCode
  }
  cf := { cf with methods := cf.methods.push mainMethod }

  let bytes := cf.toByteArray
  IO.FS.createDirAll "lean/test"
  IO.FS.writeBinFile "lean/test/StrictAndOrTest.class" bytes
  IO.println "StrictAndOrTest.class generated."
