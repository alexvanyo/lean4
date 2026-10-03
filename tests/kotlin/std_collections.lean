import Std.Data.TreeMap
import Std.Data.TreeSet
import Std.Data.HashMap
import Std.Data.HashSet
import Std.Data.ByteSlice
import Init.Data.UInt.Log2

/-!
Tests `Std.TreeMap`, `Std.TreeSet`, `Std.HashMap`, `Std.HashSet`, `ByteSlice.beq`,
`String.compare`, `String.Slice` hashing/comparison, and scalar `log2`/`abs`/`ofNatLT`
operations in the Kotlin backend.
-/

def testTreeMap (k1 k2 k3 : String) (v1 v2 v3 : Nat) : Option Nat × Option Nat × Nat :=
  let m : Std.TreeMap String Nat := {}
  let m := m.insert k1 v1
  let m := m.insert k2 v2
  let m := m.insert k3 v3
  let m := m.erase k2
  (m.get? k1, m.get? k2, m.size)

def testTreeSet (a b c : String) : Bool × Bool × Nat :=
  let s : Std.TreeSet String := {}
  let s := s.insert a
  let s := s.insert b
  let s := s.insert c
  let s := s.erase b
  (s.contains a, s.contains b, s.size)

def testHashMap (k1 k2 k3 : String) (v1 v2 v3 : Nat) : Option Nat × Option Nat × Nat :=
  let m : Std.HashMap String Nat := Std.HashMap.emptyWithCapacity 8
  let m := m.insert k1 v1
  let m := m.insert k2 v2
  let m := m.insert k3 v3
  let m := m.erase k2
  (m.get? k1, m.get? k2, m.size)

def testHashMapExpand (n : Nat) : Nat × Option Nat × Option Nat := Id.run do
  let mut m : Std.HashMap Nat Nat := Std.HashMap.emptyWithCapacity 4
  for i in [:n] do
    m := m.insert i (i * 10)
  return (m.size, m.get? 0, m.get? (n - 1))

def testHashSet (a b c : String) : Bool × Bool × Nat :=
  let s : Std.HashSet String := Std.HashSet.emptyWithCapacity 8
  let s := s.insert a
  let s := s.insert b
  let s := s.insert c
  let s := s.erase b
  (s.contains a, s.contains b, s.size)

def testScalarExtras (u8 : UInt8) (u16 : UInt16) (u32 : UInt32) (u64 : UInt64)
    (i8 : Int8) (i16 : Int16) (i32 : Int32) (i64 : Int64) : UInt64 :=
  let l8 := u8.log2.toUInt64
  let l16 := u16.log2.toUInt64
  let l32 := u32.log2.toUInt64
  let l64 := u64.log2
  let a8 := i8.abs.toUInt8.toUInt64
  let a16 := i16.abs.toUInt16.toUInt64
  let a32 := i32.abs.toUInt32.toUInt64
  let a64 := i64.abs.toUInt64
  let lt32 : UInt32 := if h : 42 < UInt32.size then UInt32.ofNatLT 42 h else 0
  l8 + l16 + l32 + l64 + a8 + a16 + a32 + a64 + lt32.toUInt64

def testFloatMinMaxNum (a b : Float) (a32 b32 : Float32) : Float :=
  let mn := Float.minimumNumber a b
  let mx := Float.maximumNumber a b
  let mn32 := Float32.minimumNumber a32 b32
  let mx32 := Float32.maximumNumber a32 b32
  mn + mx + mn32.toFloat + mx32.toFloat

def testByteSliceBeq (a b : ByteArray) : Bool :=
  let sa := ByteSlice.ofByteArray a
  let sb := ByteSlice.ofByteArray b
  sa == sb

def testStringSliceOps (a b : String) : Bool × Bool :=
  let sa := a.toSlice
  let sb := b.toSlice
  (hash sa == hash sb, decide (sa < sb))

def testDbgTrace (x : Nat) : Nat :=
  dbgTrace "trace test" (fun _ => x + 1)
