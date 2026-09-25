import Lean.Compiler.JVM

/-!
Tests wide closure capture and dynamic application on JVM.
Models closure environment capture in `tests/compile/closure_bug1.lean` and `tests/compile/large_closure_bug.lean`.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def main : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/LargeClosureTest"
    superClass := "java/lang/Object"
  }

  -- Register CP references
  let (natOfLongRef, cf') := cf.addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
  let (natAddRef, cf') := cf'.addMethodRef "lean/runtime/LeanNat" "add" "(Llean/runtime/LeanNat;Llean/runtime/LeanNat;)Llean/runtime/LeanNat;"
  let (natClassRef, cf') := cf'.addClass "lean/runtime/LeanNat"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnObjRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"
  cf := cf'

  /-
  public static LeanObject sum18(LeanObject x1, LeanObject x2, ..., LeanObject x17, LeanObject y) {
    LeanNat acc = (LeanNat) x1;
    acc = LeanNat.add(acc, (LeanNat) x2);
    ...
    acc = LeanNat.add(acc, (LeanNat) y);
    return acc;
  }
  -/
  let mut sumCode := ByteArray.empty
  sumCode := Opcode.aload 0 |>.emit sumCode
  sumCode := Opcode.checkcast natClassRef |>.emit sumCode
  sumCode := Opcode.astore 18 |>.emit sumCode -- local 18 = acc

  for i in 1...18 do
    sumCode := Opcode.aload 18 |>.emit sumCode
    sumCode := Opcode.aload i.toUInt8 |>.emit sumCode
    sumCode := Opcode.checkcast natClassRef |>.emit sumCode
    sumCode := Opcode.invokestatic natAddRef |>.emit sumCode
    sumCode := Opcode.astore 18 |>.emit sumCode

  sumCode := Opcode.aload 18 |>.emit sumCode
  sumCode := Opcode.areturn |>.emit sumCode

  let sumDescriptor := "(" ++ String.join (List.replicate 18 "Llean/runtime/LeanObject;") ++ ")Llean/runtime/LeanObject;"
  let sumMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "sum18"
    descriptor := sumDescriptor
    maxStack := 4
    maxLocals := 19
    bytecodes := sumCode
  }
  cf := { cf with methods := cf.methods.push sumMethod }

  let (sum18Ref, cf') := cf.addMethodRef "lean/test/LargeClosureTest" "sum18" sumDescriptor
  cf := cf'

  /-
  public static void main(String[] args) {
    // Call sum18(1, 2, ..., 17, 2) => 155
    System.out.println(sum18(1, 2, ..., 17, 2));
  }
  -/
  let mut mainCode := ByteArray.empty
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode

  for i in 1...18 do
    mainCode := Opcode.bipush i.toUInt8 |>.emit mainCode
    mainCode := Opcode.i2l |>.emit mainCode
    mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode

  -- 18th argument y = 2
  mainCode := Opcode.iconst_2 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode

  mainCode := Opcode.invokestatic sum18Ref |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode
  mainCode := Opcode.return_void |>.emit mainCode

  let mainMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "main"
    descriptor := "([Ljava/lang/String;)V"
    maxStack := 20
    maxLocals := 1
    bytecodes := mainCode
  }
  cf := { cf with methods := cf.methods.push mainMethod }

  let bytes := cf.toByteArray
  IO.FS.createDirAll "lean/test"
  IO.FS.writeBinFile "lean/test/LargeClosureTest.class" bytes
  IO.println "LargeClosureTest.class generated."
