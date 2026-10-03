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

def listToArrayNat (xs : List Nat) : Array Nat :=
  xs.toArray

def arrayToListNat (xs : Array Nat) : List Nat :=
  xs.toList

def listToArrayU32 (xs : List UInt32) : Array UInt32 :=
  xs.toArray

def arrayToListU32 (xs : Array UInt32) : List UInt32 :=
  xs.toList

def filterNatArray (xs : Array Nat) (minVal : Nat) : Array Nat :=
  xs.filter (· >= minVal)

def sumNatArray (xs : Array Nat) : Nat :=
  xs.foldl (· + ·) 0

def makeByteArray (a b c : UInt8) : ByteArray :=
  ByteArray.empty.push a |>.push b |>.push c

def byteArraySize (bs : ByteArray) : Nat :=
  bs.size

def byteArrayGet! (bs : ByteArray) (i : Nat) : UInt8 :=
  bs.get! i

def byteArraySet! (bs : ByteArray) (i : Nat) (v : UInt8) : ByteArray :=
  bs.markLinear.set! i v

def byteArrayAppend (a b : ByteArray) : ByteArray :=
  a ++ b

def byteArrayExtract (bs : ByteArray) (start stop : Nat) : ByteArray :=
  bs.extract start stop

def byteArrayEq (a b : ByteArray) : Bool :=
  a == b

def stringUTF8RoundTrip (s : String) : String :=
  String.fromUTF8! s.toUTF8

def byteArrayValidateUTF8 (bs : ByteArray) : Bool :=
  bs.validateUTF8

def makeFloatArray (a b c : Float) : FloatArray :=
  FloatArray.empty.push a |>.push b |>.push c

def floatArraySize (ds : FloatArray) : Nat :=
  ds.size

def floatArrayGet! (ds : FloatArray) (i : Nat) : Float :=
  ds.get! i

def floatArraySet! (ds : FloatArray) (i : Nat) (v : Float) : FloatArray :=
  ds.markLinear.set! i v

def sumFloatArray (ds : FloatArray) : Float :=
  ds.foldl (· + ·) 0.0
