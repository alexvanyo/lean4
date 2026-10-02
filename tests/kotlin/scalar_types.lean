import Lean.Compiler.Kotlin

/-!
Tests emitted Kotlin types, arithmetic, bitwise operators, and comparisons across all
supported scalar types (UInt8..UInt64, USize, Int8..Int64, ISize, Bool, Float, Float32).
-/

set_option linter.unusedVariables false

def testUInts (a : UInt8) (b : UInt16) (c : UInt32) (d : UInt64) (s : USize) : UInt64 :=
  let x8 := a + 1
  let x16 := b - 2
  let x32 := c * 3
  let x64 := d / 4
  let xShift := (d <<< 2) >>> 1
  let xBit := (d &&& 255) ||| (d ^^^ 1024)
  x64 + xShift + xBit + (if c > 0 then 100 else 0)

def testInts (a : Int8) (b : Int16) (c : Int32) (d : Int64) (s : ISize) : Int64 :=
  let y8 := a + 1
  let y16 := b - 2
  let y32 := c * 3
  let y64 := d / 4
  let yBit := (d &&& 255) ||| (d ^^^ 1024)
  y64 + yBit + (if c <= 0 then -1 else 1)

def testBools (p q : Bool) : Bool :=
  (p && q) || (!p && !q)

def testFloats (f1 : Float) (f2 : Float32) : Float :=
  let sum := f1 + 1.5
  let diff := sum - 0.5
  let prod := diff * 2.0
  let quot := prod / 4.0
  let neg := -quot
  let from32 := f2.toFloat
  let c := if f1 < 10.0 && f1 <= 10.0 && !(f1 == 0.0) then from32 else 0.0
  Float.abs neg + Float.sqrt 9.0 + Float.floor 2.7 + Float.ceil 2.3 + c

def testFloat32s (a b : Float32) : Float32 :=
  let x := (a + b - 0.5) * 2.0 / 4.0
  let y := -x
  if a < b && a <= b && !(a == b) then Float32.abs y else 0.0

def testLeanInts (a b : Int) : Int :=
  let sum := a + b
  let diff := sum - 3
  let prod := diff * (-2)
  let q := Int.tdiv prod 3
  let r := Int.tmod prod 3
  let eq := a / b
  let er := a % b
  let neg := -q
  let absVal : Int := Int.ofNat (Int.natAbs a)
  if a == b || a < b || a <= b then
    neg + r + eq + er + absVal
  else
    neg - r + absVal

