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
