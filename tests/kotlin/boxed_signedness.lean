import Lean.Compiler.Kotlin

/-!
Tests that fixed-width integers in generic (`Any?`) slots are read back correctly whichever
signedness they were boxed with. The IR does not keep `Int32` and `UInt32` apart (`UInt32.toInt32`
is erased), so a value written as `UInt` may be read as `Int` and vice versa.
-/

namespace BoxedSignedness

def fmt (x y : Int32) : String := s!"{x} {y}"

/-- Elements of an `Array (Int32 × Int32)`, read before an `Int32` parameter. -/
def formatAt (cells : Array (Int32 × Int32)) (i : UInt32) : String :=
  let (x, y) := cells[i.toNat]!
  fmt x y

/-- The erased proof argument creates a `_redArg` copy that reads the element as `UInt32`. -/
def firstMinusOne (cells : Array (Int32 × Int32)) (_h : True) (i : UInt32) : Int32 :=
  let c := cells[i.toNat]!
  c.1 - 1

def callFirstMinusOne (cells : Array (Int32 × Int32)) : Int32 := firstMinusOne cells trivial 0

structure Parsed where
  val : Int32
  next : UInt32

/-- The `UInt32` is stored unconverted into the `Int32` field. -/
@[noinline] def mkParsed (v : UInt32) (e : UInt32) : Option Parsed :=
  if e == 0 then none else some ⟨v.toInt32, e⟩

def parsedVal (v e : UInt32) : Int32 :=
  match mkParsed v e with
  | some p => p.val
  | none => 0

@[noinline] def parsePair (b : Bool) (x y : Int32) : Option (Int32 × Int32) :=
  if b then some (x, y) else none

/-- The first component of the `some` payload, not the payload itself. -/
def sumPair (b : Bool) (x y : Int32) : Int32 :=
  match parsePair b x y with
  | some (p, q) => p + q
  | none => 0

@[kotlin_class "SignedWarn"]
structure SignedWarn where
  tag : UInt32
  charVal : UInt32
  offsetX : Int32
  offsetY : Int32

/-- The `0` literals are shared between the `UInt32` and `Int32` fields. -/
def mkSignedWarn (t : UInt32) : SignedWarn := ⟨t, 0, 0, 0⟩

end BoxedSignedness
