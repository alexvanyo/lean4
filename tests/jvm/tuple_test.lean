import Lean.Compiler.JVM

/-!
Tests constructor field projections and nested tuples on JVM.
Models product projection in `tests/compile/tuple.lean`.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def main : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/TupleTest"
    superClass := "java/lang/Object"
  }

  -- Register CP references
  let (ctorAllocRef, cf') := cf.addMethodRef "lean/runtime/LeanCtor" "alloc" "(III)Llean/runtime/LeanCtor;"
  let (ctorSetObjRef, cf') := cf'.addMethodRef "lean/runtime/LeanCtor" "setObj" "(ILlean/runtime/LeanObject;)V"
  let (ctorGetObjRef, cf') := cf'.addMethodRef "lean/runtime/LeanCtor" "getObj" "(I)Llean/runtime/LeanObject;"
  let (ctorClassRef, cf') := cf'.addClass "lean/runtime/LeanCtor"
  let (natOfLongRef, cf') := cf'.addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
  let (natAddRef, cf') := cf'.addMethodRef "lean/runtime/LeanNat" "add" "(Llean/runtime/LeanNat;Llean/runtime/LeanNat;)Llean/runtime/LeanNat;"
  let (natClassRef, cf') := cf'.addClass "lean/runtime/LeanNat"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"
  cf := cf'

  /-
  public static LeanObject f(LeanObject a) {
    LeanCtor ctorA = (LeanCtor) a;
    LeanNat fst = (LeanNat) ctorA.getObj(0);
    LeanCtor snd = (LeanCtor) ctorA.getObj(1);
    LeanNat sndFst = (LeanNat) snd.getObj(0);
    LeanNat sndSnd = (LeanNat) snd.getObj(1);
    LeanNat sum1 = LeanNat.add(fst, sndFst);
    return LeanNat.add(sum1, sndSnd);
  }
  -/
  let mut fCode := ByteArray.empty
  -- ctorA = (LeanCtor) a
  fCode := Opcode.aload 0 |>.emit fCode
  fCode := Opcode.checkcast ctorClassRef |>.emit fCode
  fCode := Opcode.astore 1 |>.emit fCode

  -- fst = (LeanNat) ctorA.getObj(0)
  fCode := Opcode.aload 1 |>.emit fCode
  fCode := Opcode.iconst_0 |>.emit fCode
  fCode := Opcode.invokevirtual ctorGetObjRef |>.emit fCode
  fCode := Opcode.checkcast natClassRef |>.emit fCode
  fCode := Opcode.astore 2 |>.emit fCode

  -- snd = (LeanCtor) ctorA.getObj(1)
  fCode := Opcode.aload 1 |>.emit fCode
  fCode := Opcode.iconst_1 |>.emit fCode
  fCode := Opcode.invokevirtual ctorGetObjRef |>.emit fCode
  fCode := Opcode.checkcast ctorClassRef |>.emit fCode
  fCode := Opcode.astore 3 |>.emit fCode

  -- sndFst = (LeanNat) snd.getObj(0)
  fCode := Opcode.aload 3 |>.emit fCode
  fCode := Opcode.iconst_0 |>.emit fCode
  fCode := Opcode.invokevirtual ctorGetObjRef |>.emit fCode
  fCode := Opcode.checkcast natClassRef |>.emit fCode
  fCode := Opcode.astore 4 |>.emit fCode

  -- sndSnd = (LeanNat) snd.getObj(1)
  fCode := Opcode.aload 3 |>.emit fCode
  fCode := Opcode.iconst_1 |>.emit fCode
  fCode := Opcode.invokevirtual ctorGetObjRef |>.emit fCode
  fCode := Opcode.checkcast natClassRef |>.emit fCode
  fCode := Opcode.astore 5 |>.emit fCode

  -- sum1 = LeanNat.add(fst, sndFst)
  fCode := Opcode.aload 2 |>.emit fCode
  fCode := Opcode.aload 4 |>.emit fCode
  fCode := Opcode.invokestatic natAddRef |>.emit fCode
  -- return LeanNat.add(sum1, sndSnd)
  fCode := Opcode.aload 5 |>.emit fCode
  fCode := Opcode.invokestatic natAddRef |>.emit fCode
  fCode := Opcode.areturn |>.emit fCode

  let fMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "f"
    descriptor := "(Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
    maxStack := 4
    maxLocals := 6
    bytecodes := fCode
  }
  cf := { cf with methods := cf.methods.push fMethod }

  let (fRef, cf') := cf.addMethodRef "lean/test/TupleTest" "f" "(Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
  cf := cf'

  /-
  public static void main(String[] args) {
    // inner = (2, 3)
    LeanCtor inner = LeanCtor.alloc(0, 2, 0);
    inner.setObj(0, LeanNat.ofLong(2L));
    inner.setObj(1, LeanNat.ofLong(3L));

    // outer = (1, inner)
    LeanCtor outer = LeanCtor.alloc(0, 2, 0);
    outer.setObj(0, LeanNat.ofLong(1L));
    outer.setObj(1, inner);

    // print f(outer) => 6
    System.out.println(f(outer));
  }
  -/
  let mut mainCode := ByteArray.empty
  -- inner = LeanCtor.alloc(0, 2, 0)
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.iconst_2 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.invokestatic ctorAllocRef |>.emit mainCode
  mainCode := Opcode.astore 1 |>.emit mainCode

  -- inner.setObj(0, LeanNat.ofLong(2L))
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.iconst_2 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetObjRef |>.emit mainCode

  -- inner.setObj(1, LeanNat.ofLong(3L))
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.iconst_3 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetObjRef |>.emit mainCode

  -- outer = LeanCtor.alloc(0, 2, 0)
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.iconst_2 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.invokestatic ctorAllocRef |>.emit mainCode
  mainCode := Opcode.astore 2 |>.emit mainCode

  -- outer.setObj(0, LeanNat.ofLong(1L))
  mainCode := Opcode.aload 2 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetObjRef |>.emit mainCode

  -- outer.setObj(1, inner)
  mainCode := Opcode.aload 2 |>.emit mainCode
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetObjRef |>.emit mainCode

  -- System.out.println(f(outer))
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 2 |>.emit mainCode
  mainCode := Opcode.invokestatic fRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnRef |>.emit mainCode
  mainCode := Opcode.return_void |>.emit mainCode

  let mainMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "main"
    descriptor := "([Ljava/lang/String;)V"
    maxStack := 4
    maxLocals := 3
    bytecodes := mainCode
  }
  cf := { cf with methods := cf.methods.push mainMethod }

  let bytes := cf.toByteArray
  IO.FS.createDirAll "lean/test"
  IO.FS.writeBinFile "lean/test/TupleTest.class" bytes
  IO.println "TupleTest.class generated."
