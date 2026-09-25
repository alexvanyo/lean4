import Lean.Compiler.JVM

/-!
Tests recursive inductive data type constructors and pattern tags on JVM.
Models symbolic AST construction in `tests/compile/reusebug.lean`.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def main : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/ReuseBugTest"
    superClass := "java/lang/Object"
  }

  let (ctorAllocRef, cf') := cf.addMethodRef "lean/runtime/LeanCtor" "alloc" "(III)Llean/runtime/LeanCtor;"
  let (ctorSetObjRef, cf') := cf'.addMethodRef "lean/runtime/LeanCtor" "setObj" "(ILlean/runtime/LeanObject;)V"
  let (ctorGetObjRef, cf') := cf'.addMethodRef "lean/runtime/LeanCtor" "getObj" "(I)Llean/runtime/LeanObject;"
  let (ctorGetTagRef, cf') := cf'.addMethodRef "lean/runtime/LeanObject" "getTag" "()I"
  let (strOfRef, cf') := cf'.addMethodRef "lean/runtime/LeanString" "of" "(Ljava/lang/String;)Llean/runtime/LeanString;"
  let (natOfLongRef, cf') := cf'.addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
  let (strXRef, cf') := cf'.addString "x"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnIntRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(I)V"
  cf := cf'

  /-
  public static void main(String[] args) {
    // x = Var("x") (tag 1, 1 obj)
    LeanCtor x = LeanCtor.alloc(1, 1, 0);
    x.setObj(0, LeanString.of("x"));

    // v2 = Val(2) (tag 0, 1 obj)
    LeanCtor v2 = LeanCtor.alloc(0, 1, 0);
    v2.setObj(0, LeanNat.ofLong(2L));

    // m = Mul(v2, x) (tag 3, 2 objs)
    LeanCtor m = LeanCtor.alloc(3, 2, 0);
    m.setObj(0, v2);
    m.setObj(1, x);

    // inner = Add(m, x) (tag 2, 2 objs)
    LeanCtor inner = LeanCtor.alloc(2, 2, 0);
    inner.setObj(0, m);
    inner.setObj(1, x);

    // v1 = Val(1) (tag 0, 1 obj)
    LeanCtor v1 = LeanCtor.alloc(0, 1, 0);
    v1.setObj(0, LeanNat.ofLong(1L));

    // top = Add(v1, inner) (tag 2, 2 objs)
    LeanCtor top = LeanCtor.alloc(2, 2, 0);
    top.setObj(0, v1);
    top.setObj(1, inner);

    System.out.println(top.getTag());              // 2
    System.out.println(top.getObj(0).getTag());    // 0
    System.out.println(top.getObj(1).getTag());    // 2
  }
  -/
  let mut mainCode := ByteArray.empty
  -- x = LeanCtor.alloc(1, 1, 0)
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.invokestatic ctorAllocRef |>.emit mainCode
  mainCode := Opcode.astore 1 |>.emit mainCode -- local 1 = x
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.ldc strXRef.toUInt8 |>.emit mainCode
  mainCode := Opcode.invokestatic strOfRef |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetObjRef |>.emit mainCode

  -- v2 = LeanCtor.alloc(0, 1, 0)
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.invokestatic ctorAllocRef |>.emit mainCode
  mainCode := Opcode.astore 2 |>.emit mainCode -- local 2 = v2
  mainCode := Opcode.aload 2 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.iconst_2 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetObjRef |>.emit mainCode

  -- m = LeanCtor.alloc(3, 2, 0)
  mainCode := Opcode.iconst_3 |>.emit mainCode
  mainCode := Opcode.iconst_2 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.invokestatic ctorAllocRef |>.emit mainCode
  mainCode := Opcode.astore 3 |>.emit mainCode -- local 3 = m
  mainCode := Opcode.aload 3 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.aload 2 |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetObjRef |>.emit mainCode
  mainCode := Opcode.aload 3 |>.emit mainCode
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetObjRef |>.emit mainCode

  -- inner = LeanCtor.alloc(2, 2, 0)
  mainCode := Opcode.iconst_2 |>.emit mainCode
  mainCode := Opcode.iconst_2 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.invokestatic ctorAllocRef |>.emit mainCode
  mainCode := Opcode.astore 4 |>.emit mainCode -- local 4 = inner
  mainCode := Opcode.aload 4 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.aload 3 |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetObjRef |>.emit mainCode
  mainCode := Opcode.aload 4 |>.emit mainCode
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetObjRef |>.emit mainCode

  -- v1 = LeanCtor.alloc(0, 1, 0)
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.invokestatic ctorAllocRef |>.emit mainCode
  mainCode := Opcode.astore 5 |>.emit mainCode -- local 5 = v1
  mainCode := Opcode.aload 5 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetObjRef |>.emit mainCode

  -- top = LeanCtor.alloc(2, 2, 0)
  mainCode := Opcode.iconst_2 |>.emit mainCode
  mainCode := Opcode.iconst_2 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.invokestatic ctorAllocRef |>.emit mainCode
  mainCode := Opcode.astore 6 |>.emit mainCode -- local 6 = top
  mainCode := Opcode.aload 6 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.aload 5 |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetObjRef |>.emit mainCode
  mainCode := Opcode.aload 6 |>.emit mainCode
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.aload 4 |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetObjRef |>.emit mainCode

  -- System.out.println(top.getTag())
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 6 |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorGetTagRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnIntRef |>.emit mainCode

  -- System.out.println(top.getObj(0).getTag())
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 6 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorGetObjRef |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorGetTagRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnIntRef |>.emit mainCode

  -- System.out.println(top.getObj(1).getTag())
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 6 |>.emit mainCode
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorGetObjRef |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorGetTagRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnIntRef |>.emit mainCode

  mainCode := Opcode.return_void |>.emit mainCode

  let mainMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "main"
    descriptor := "([Ljava/lang/String;)V"
    maxStack := 4
    maxLocals := 7
    bytecodes := mainCode
  }
  cf := { cf with methods := cf.methods.push mainMethod }

  IO.FS.createDirAll "lean/test"
  IO.FS.writeBinFile "lean/test/ReuseBugTest.class" cf.toByteArray
  IO.println "ReuseBugTest.class generated."
