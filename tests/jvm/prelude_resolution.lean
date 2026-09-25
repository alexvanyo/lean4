import Lean.Compiler.JVM

/-!
Tests resolution and execution of standard library `Init.*` prelude operations on JVM.
Verifies that `Option`, `UInt64`, `Nat` comparison, and `String.length` execute
via standard runtime modules without requiring manual user shims.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def main : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/PreludeResolutionTest"
    superClass := "java/lang/Object"
  }

  let (natOfLongRef, cf') := cf.addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
  let (strOfRef, cf') := cf'.addMethodRef "lean/runtime/LeanString" "of" "(Ljava/lang/String;)Llean/runtime/LeanString;"
  let (ctorAllocRef, cf') := cf'.addMethodRef "lean/runtime/LeanCtor" "alloc" "(III)Llean/runtime/LeanCtor;"
  let (ctorSetObjRef, cf') := cf'.addMethodRef "lean/runtime/LeanCtor" "setObj" "(ILlean/runtime/LeanObject;)V"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnObjRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"

  -- Prelude methods
  let (uint64DecEqRef, cf') := cf'.addMethodRef "lean/mod_l_Init_Prelude" "f_UInt64_decEq" "(Llean/runtime/LeanObject;Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
  let (natDecLeRef, cf') := cf'.addMethodRef "lean/mod_l_Init_Prelude" "f_Nat_decLe" "(Llean/runtime/LeanObject;Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
  let (strLenRef, cf') := cf'.addMethodRef "lean/mod_l_Init_Data_String_Bootstrap" "f_String_Internal_length" "(Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
  let (natShiftRightRef, cf') := cf'.addMethodRef "lean/mod_l_Init_Data_Nat_Bitwise_Basic" "f_Nat_shiftRight" "(Llean/runtime/LeanObject;Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
  let (natReprRef, cf') := cf'.addMethodRef "lean/mod_l_Init_Data_Repr" "f_Nat_reprFast" "(Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
  let (optDecEqRef, cf') := cf'.addMethodRef "lean/mod_l_Init_Data_Option_Basic" "f_Option_instDecidableEq___redArg" "(Llean/runtime/LeanObject;Llean/runtime/LeanObject;Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
  let (strHelloRef, cf') := cf'.addString "hello"
  cf := cf'

  /-
  public static void main(String[] args) {
    // f_UInt64_decEq(10, 10) => 1
    System.out.println(mod_l_Init_Prelude.f_UInt64_decEq(LeanNat.ofLong(10), LeanNat.ofLong(10)));
    // f_Nat_decLe(5, 10) => 1
    System.out.println(mod_l_Init_Prelude.f_Nat_decLe(LeanNat.ofLong(5), LeanNat.ofLong(10)));
    // f_Nat_decLe(10, 5) => 0
    System.out.println(mod_l_Init_Prelude.f_Nat_decLe(LeanNat.ofLong(10), LeanNat.ofLong(5)));
    // f_String_Internal_length("hello") => 5
    System.out.println(mod_l_Init_Data_String_Bootstrap.f_String_Internal_length(LeanString.of("hello")));
    // f_Nat_shiftRight(64, 2) => 16
    System.out.println(mod_l_Init_Data_Nat_Bitwise_Basic.f_Nat_shiftRight(LeanNat.ofLong(64), LeanNat.ofLong(2)));
    // f_Nat_reprFast(123) => "123"
    System.out.println(mod_l_Init_Data_Repr.f_Nat_reprFast(LeanNat.ofLong(123)));
  }
  -/
  let mut mainCode := ByteArray.empty

  -- 1. f_UInt64_decEq(10, 10)
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.bipush 10 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.bipush 10 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokestatic uint64DecEqRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode

  -- 2. f_Nat_decLe(5, 10)
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.iconst_5 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.bipush 10 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokestatic natDecLeRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode

  -- 3. f_Nat_decLe(10, 5)
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.bipush 10 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.iconst_5 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokestatic natDecLeRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode

  -- 4. f_String_Internal_length("hello")
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.ldc strHelloRef.toUInt8 |>.emit mainCode
  mainCode := Opcode.invokestatic strOfRef |>.emit mainCode
  mainCode := Opcode.invokestatic strLenRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode

  -- 5. f_Nat_shiftRight(64, 2)
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.bipush 64 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.iconst_2 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokestatic natShiftRightRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode

  -- 6. f_Nat_reprFast(123)
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.bipush 123 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokestatic natReprRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode

  -- 7. Option eq: some(42) == some(42)
  -- c1 = some(42) (tag 1, obj 42)
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.invokestatic ctorAllocRef |>.emit mainCode
  mainCode := Opcode.dup |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.bipush 42 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetObjRef |>.emit mainCode
  mainCode := Opcode.astore 1 |>.emit mainCode -- local 1 = c1

  -- c2 = some(42)
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.invokestatic ctorAllocRef |>.emit mainCode
  mainCode := Opcode.dup |>.emit mainCode
  mainCode := Opcode.iconst_0 |>.emit mainCode
  mainCode := Opcode.bipush 42 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokevirtual ctorSetObjRef |>.emit mainCode
  mainCode := Opcode.astore 2 |>.emit mainCode -- local 2 = c2

  -- System.out.println(optDecEq(null, c1, c2)) => 1
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aconst_null |>.emit mainCode
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.aload 2 |>.emit mainCode
  mainCode := Opcode.invokestatic optDecEqRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode

  mainCode := Opcode.return_void |>.emit mainCode

  let mainMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "main"
    descriptor := "([Ljava/lang/String;)V"
    maxStack := 5
    maxLocals := 3
    bytecodes := mainCode
  }
  cf := { cf with methods := cf.methods.push mainMethod }

  IO.FS.createDirAll "lean/test"
  IO.FS.writeBinFile "lean/test/PreludeResolutionTest.class" cf.toByteArray
  IO.println "PreludeResolutionTest.class generated."
