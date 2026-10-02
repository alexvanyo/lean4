import Lean.Compiler.Kotlin

/-!
Tests Kotlin typed array emission (LongArray, UIntArray, ByteArray, BooleanArray)
enabled by `compiler.kotlin.typedArrays = true`.
-/

set_option linter.unusedVariables false

def makeU64Array (n : Nat) : Array UInt64 :=
  Array.replicate n 0

def makeU32Array (n : Nat) (initVal : UInt32) : Array UInt32 :=
  Array.replicate n initVal

def makeBoolArray (n : Nat) : Array Bool :=
  Array.replicate n false

def readArrayU64 (a : Array UInt64) (i : USize) (h : i.toNat < a.size) : UInt64 :=
  a.uget i h

def getArraySize (a : Array UInt64) : Nat :=
  a.size

def swapArrayU32 (a : Array UInt32) (i j : Nat) (hi : i < a.size) (hj : j < a.size) : Array UInt32 :=
  a.swap i j

def swapIfInBoundsU32 (a : Array UInt32) (i j : Nat) : Array UInt32 :=
  a.swapIfInBounds i j

def pushPopU32 (a : Array UInt32) (x y : UInt32) : Array UInt32 :=
  let a1 := a.push x
  let a2 := a1.push y
  a2.pop

def makeLiteralU32 (a b c : UInt32) : Array UInt32 :=
  #[a, b, c]

def appendU32 (a b : Array UInt32) : Array UInt32 :=
  a ++ b

def sliceU32 (a : Array UInt32) (start stop : Nat) : Array UInt32 :=
  a.extract start stop

def makeLiteralStr (a b : String) : Array String :=
  #[a, b]

