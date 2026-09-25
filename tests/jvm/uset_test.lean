import Lean.Compiler.JVM

/-!
Tests scalar field projections and scalar mutation on JVM.
Models USize/UInt scalar field access and record update in `tests/compile/uset.lean`.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def main : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/USetTest"
    superClass := "java/lang/Object"
  }

  -- Register CP references
  let (ctorAllocRef, cf') := cf.addMethodRef "lean/runtime/LeanCtor" "alloc" "(III)Llean/runtime/LeanCtor;"
  let (ctorGetScalarRef, cf') := cf'.addMethodRef "lean/runtime/LeanCtor" "getScalar" "(I)J"
  let (ctorSetScalarRef, cf') := cf'.addMethodRef "lean/runtime/LeanCtor" "setScalar" "(IJ)V"
  let (_, cf') := cf'.addClass "lean/runtime/LeanCtor"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnLongRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(J)V"
  cf := cf'

  /-
  public static LeanCtor pointRight(LeanCtor p) {
    LeanCtor p2 = LeanCtor.alloc(0, 0, 2);
    long oldX = p.getScalar(0);
    p2.setScalar(0, oldX + 1L);
    p2.setScalar(1, p.getScalar(1));
    return p2;
  }
  -/
  let mut rightCode := ByteArray.empty
  -- p2 = LeanCtor.alloc(0, 0, 2)
  rightCode := Opcode.iconst_0 |>.emit rightCode
  rightCode := Opcode.iconst_0 |>.emit rightCode
  rightCode := Opcode.iconst_2 |>.emit rightCode
  rightCode := Opcode.invokestatic ctorAllocRef |>.emit rightCode
  rightCode := Opcode.astore 1 |>.emit rightCode -- local 1 = p2

  -- p2.setScalar(0, p.getScalar(0) + 1L)
  rightCode := Opcode.aload 1 |>.emit rightCode
  rightCode := Opcode.iconst_0 |>.emit rightCode
  rightCode := Opcode.aload 0 |>.emit rightCode
  rightCode := Opcode.iconst_0 |>.emit rightCode
  rightCode := Opcode.invokevirtual ctorGetScalarRef |>.emit rightCode
  rightCode := Opcode.lconst_1 |>.emit rightCode
  rightCode := Opcode.ladd |>.emit rightCode
  rightCode := Opcode.invokevirtual ctorSetScalarRef |>.emit rightCode

  -- p2.setScalar(1, p.getScalar(1))
  rightCode := Opcode.aload 1 |>.emit rightCode
  rightCode := Opcode.iconst_1 |>.emit rightCode
  rightCode := Opcode.aload 0 |>.emit rightCode
  rightCode := Opcode.iconst_1 |>.emit rightCode
  rightCode := Opcode.invokevirtual ctorGetScalarRef |>.emit rightCode
  rightCode := Opcode.invokevirtual ctorSetScalarRef |>.emit rightCode

  rightCode := Opcode.aload 1 |>.emit rightCode
  rightCode := Opcode.areturn |>.emit rightCode

  let rightMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "pointRight"
    descriptor := "(Llean/runtime/LeanCtor;)Llean/runtime/LeanCtor;"
    maxStack := 8
    maxLocals := 2
    bytecodes := rightCode
  }
  cf := { cf with methods := cf.methods.push rightMethod }

  let (pointRightRef, cf') := cf.addMethodRef "lean/test/USetTest" "pointRight" "(Llean/runtime/LeanCtor;)Llean/runtime/LeanCtor;"
  cf := cf'

  /-
  public static void main(String[] args) {
    LeanCtor p = LeanCtor.alloc(0, 0, 2);
    p.setScalar(0, 41L);
    p.setScalar(1, 100L);
    LeanCtor p2 = pointRight(p);
    System.out.println(p2.getScalar(0)); // 42
  }
  -/
  let mut mainCode := ByteArray.empty
  -- p = LeanCtor.alloc(0, 0, 2)
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.iconst_2 |>.emit mainCode
  mainCode := Opcode.invokestatic ctorAllocRef |>.emit mainCode
  mainCode := Opcode.astore 1 |>.emit mainCode -- local 1 = p

  -- p.setScalar(0, 41L)
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.bipush 41 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetScalarRef |>.emit mainCode

  -- p.setScalar(1, 100L)
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.bipush 100 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetScalarRef |>.emit mainCode

  -- p2 = pointRight(p)
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.invokestatic pointRightRef |>.emit mainCode
  mainCode := Opcode.astore 2 |>.emit mainCode -- local 2 = p2

  -- System.out.println(p2.getScalar(0))
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 2 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorGetScalarRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnLongRef |>.emit mainCode
  mainCode := Opcode.return_void |>.emit mainCode

  let mainMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "main"
    descriptor := "([Ljava/lang/String;)V"
    maxStack := 8
    maxLocals := 3
    bytecodes := mainCode
  }
  cf := { cf with methods := cf.methods.push mainMethod }

  let bytes := cf.toByteArray
  IO.FS.createDirAll "lean/test"
  IO.FS.writeBinFile "lean/test/USetTest.class" bytes
  IO.println "USetTest.class generated."
