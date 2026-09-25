import Lean.Compiler.JVM

/-!
Tests multi-word and boundary bitwise shift operations (`f_Nat_shiftRight`) on JVM.
Adapted from `tests/compile/nat_shiftr.lean`, covering 8-bit, 16-bit, 32-bit, 64-bit,
and multi-word large Nat shifts.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def main : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/NatShiftTest"
    superClass := "java/lang/Object"
  }

  let (natOfLongRef, cf') := cf.addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
  let (bigIntCtorRef, cf') := cf'.addMethodRef "java/math/BigInteger" "<init>" "(Ljava/lang/String;)V"
  let (bigIntClassRef, cf') := cf'.addClass "java/math/BigInteger"
  let (natOfBigIntRef, cf') := cf'.addMethodRef "lean/runtime/LeanNat" "ofBigInteger" "(Ljava/math/BigInteger;)Llean/runtime/LeanNat;"
  let (shiftRightRef, cf') := cf'.addMethodRef "lean/mod_l_Init_Data_Nat_Bitwise_Basic" "f_Nat_shiftRight" "(Llean/runtime/LeanObject;Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnObjRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"
  let (str2Pow32Ref, cf') := cf'.addString "4294967296"
  let (str2Pow64Ref, cf') := cf'.addString "18446744073709551616" -- 2^64 = 0x1_0000_0000_0000_0000
  cf := cf'

  let mut mainCode := ByteArray.empty

  -- 1. Small Nat shift: 256 >>> 4 = 16
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.sipush 256 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.iconst_4 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokestatic shiftRightRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode

  -- 2. 32-bit boundary: 0x1_0000_0000 (4294967296) >>> 32 = 1
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.new bigIntClassRef |>.emit mainCode
  mainCode := Opcode.dup |>.emit mainCode
  mainCode := Opcode.ldc str2Pow32Ref.toUInt8 |>.emit mainCode
  mainCode := Opcode.invokespecial bigIntCtorRef |>.emit mainCode
  mainCode := Opcode.invokestatic natOfBigIntRef |>.emit mainCode
  mainCode := Opcode.bipush 32 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokestatic shiftRightRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode

  -- 3. Multi-word Nat (BigInteger): 2^64 >>> 63 = 2
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.new bigIntClassRef |>.emit mainCode
  mainCode := Opcode.dup |>.emit mainCode
  mainCode := Opcode.ldc str2Pow64Ref.toUInt8 |>.emit mainCode
  mainCode := Opcode.invokespecial bigIntCtorRef |>.emit mainCode
  mainCode := Opcode.invokestatic natOfBigIntRef |>.emit mainCode
  mainCode := Opcode.bipush 63 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokestatic shiftRightRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode

  -- 4. Multi-word Nat (BigInteger): 2^64 >>> 64 = 1
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.new bigIntClassRef |>.emit mainCode
  mainCode := Opcode.dup |>.emit mainCode
  mainCode := Opcode.ldc str2Pow64Ref.toUInt8 |>.emit mainCode
  mainCode := Opcode.invokespecial bigIntCtorRef |>.emit mainCode
  mainCode := Opcode.invokestatic natOfBigIntRef |>.emit mainCode
  mainCode := Opcode.bipush 64 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokestatic shiftRightRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode

  -- 5. Multi-word Nat (BigInteger): 2^64 >>> 65 = 0
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.new bigIntClassRef |>.emit mainCode
  mainCode := Opcode.dup |>.emit mainCode
  mainCode := Opcode.ldc str2Pow64Ref.toUInt8 |>.emit mainCode
  mainCode := Opcode.invokespecial bigIntCtorRef |>.emit mainCode
  mainCode := Opcode.invokestatic natOfBigIntRef |>.emit mainCode
  mainCode := Opcode.bipush 65 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokestatic shiftRightRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode

  mainCode := Opcode.return_void |>.emit mainCode

  let mainMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "main"
    descriptor := "([Ljava/lang/String;)V"
    maxStack := 5
    maxLocals := 2
    bytecodes := mainCode
  }
  cf := { cf with methods := cf.methods.push mainMethod }

  IO.FS.createDirAll "lean/test"
  IO.FS.writeBinFile "lean/test/NatShiftTest.class" cf.toByteArray
  IO.println "NatShiftTest.class generated."
