import Lean.Compiler.Kotlin

/-!
Tests integer scalar conversion functions (ofNat, toNat, toInt, toUInt8..toUInt64,
toInt8..toInt64, toUSize, toISize, and toNatClampNeg) in the Kotlin backend.
-/

set_option linter.unusedVariables false

def testConversions (n : Nat) (i : Int) (u64 : UInt64) (i64 : Int64) : Nat :=
  let u8 : UInt8 := UInt8.ofNat n
  let u16 : UInt16 := UInt16.ofNat n
  let u32 : UInt32 := UInt32.ofNat n
  let s8 : Int8 := Int8.ofInt i
  let s16 : Int16 := Int16.ofInt i
  let s32 : Int32 := Int32.ofInt i
  let c1 : Nat := u8.toNat
  let c2 : Nat := u16.toNat
  let c3 : Nat := u32.toNat
  let c4 : Nat := u64.toNat
  let c5 : Nat := s8.toNatClampNeg
  let c6 : Nat := i64.toNatClampNeg
  c1 + c2 + c3 + c4 + c5 + c6

def testDirectCast (a : UInt32) (b : Int32) : UInt64 :=
  let b_as_u : UInt64 := UInt64.ofNat (Int.toNat b.toInt)
  let a_as_u : UInt64 := a.toUInt64
  a_as_u + b_as_u

def testFloatConversions (f : Float) (f32 : Float32) (u32 : UInt32) (i32 : Int32) : Float :=
  let u : UInt32 := f.toUInt32
  let s : Int32 := f.toInt32
  let s32 : Int32 := f32.toInt32
  let f_from_u : Float := u.toFloat + u32.toFloat
  let f_from_s : Float := s.toFloat + i32.toFloat + s32.toFloat
  let f32_sum : Float32 := f.toFloat32 + u32.toFloat32 + i32.toFloat32
  f_from_u + f_from_s + f32_sum.toFloat

