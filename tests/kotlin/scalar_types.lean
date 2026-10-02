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
  f1 + 1.5
