import Lean.Compiler.JVM

/-!
Tests wide constructor allocation and arbitrary field access on JVM.
Models the 70-field structure test in `tests/compile/bigctor.lean`.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def main : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/BigCtorTest"
    superClass := "java/lang/Object"
  }

  -- Register CP references
  let (ctorAllocRef, cf') := cf.addMethodRef "lean/runtime/LeanCtor" "alloc" "(III)Llean/runtime/LeanCtor;"
  let (ctorSetObjRef, cf') := cf'.addMethodRef "lean/runtime/LeanCtor" "setObj" "(ILlean/runtime/LeanObject;)V"
  let (ctorGetObjRef, cf') := cf'.addMethodRef "lean/runtime/LeanCtor" "getObj" "(I)Llean/runtime/LeanObject;"
  let (natOfLongRef, cf') := cf'.addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"
  cf := cf'

  /-
  public static void main(String[] args) {
    // Allocate 70-field constructor
    LeanCtor foo = LeanCtor.alloc(0, 70, 0);
    // Set field 59 (yy10) = 42
    foo.setObj(59, LeanNat.ofLong(42L));
    // Print foo.getObj(59) => 42
    System.out.println(foo.getObj(59));
  }
  -/
  let mut mainCode := ByteArray.empty
  -- LeanCtor.alloc(0, 70, 0)
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.bipush 70 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.invokestatic ctorAllocRef |>.emit mainCode
  mainCode := Opcode.astore 1 |>.emit mainCode -- local 1 = foo

  -- foo.setObj(59, LeanNat.ofLong(42L))
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.bipush 59 |>.emit mainCode
  mainCode := Opcode.bipush 42 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetObjRef |>.emit mainCode

  -- System.out.println(foo.getObj(59))
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.bipush 59 |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorGetObjRef |>.emit mainCode
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
  IO.FS.writeBinFile "lean/test/BigCtorTest.class" bytes
  IO.println "BigCtorTest.class generated."
