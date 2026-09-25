import Lean.Compiler.JVM

/-!
Tests lazy Thunk evaluation and memoization on JVM.
Models delayed computation in `tests/compile/thunk.lean`.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def main : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/ThunkTest"
    superClass := "java/lang/Object"
  }

  -- Register CP references
  let (thunkPureRef, cf') := cf.addMethodRef "lean/runtime/LeanThunk" "pure" "(Llean/runtime/LeanObject;)Llean/runtime/LeanThunk;"
  let (thunkGetRef, cf') := cf'.addMethodRef "lean/runtime/LeanThunk" "get" "()Llean/runtime/LeanObject;"
  let (natOfLongRef, cf') := cf'.addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnObjRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"
  cf := cf'

  /-
  public static void main(String[] args) {
    LeanThunk t = LeanThunk.pure(LeanNat.ofLong(42L));
    System.out.println(t.get()); // 42
  }
  -/
  let mut mainCode := ByteArray.empty
  -- LeanNat.ofLong(42L)
  mainCode := Opcode.bipush 42 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  -- LeanThunk.pure(...)
  mainCode := Opcode.invokestatic thunkPureRef |>.emit mainCode
  mainCode := Opcode.astore 1 |>.emit mainCode -- local 1 = t

  -- System.out.println(t.get())
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.invokevirtual thunkGetRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode
  mainCode := Opcode.return_void |>.emit mainCode

  let mainMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "main"
    descriptor := "([Ljava/lang/String;)V"
    maxStack := 3
    maxLocals := 2
    bytecodes := mainCode
  }
  cf := { cf with methods := cf.methods.push mainMethod }

  let bytes := cf.toByteArray
  IO.FS.createDirAll "lean/test"
  IO.FS.writeBinFile "lean/test/ThunkTest.class" bytes
  IO.println "ThunkTest.class generated."
