import Lean.Compiler.Kotlin

/-!
Tests structure field projections in the Kotlin backend.
Adapted from `tests/compile/tuple.lean` for @[kotlin_class] field property accesses.
-/

set_option linter.unusedVariables false

@[kotlin_class "Triple"]
structure Triple where
  fst : Int32
  snd : Int32
  thd : Int32

@[kotlin_member "Triple" "public" "sumFields"]
def Triple.sumFields (t : Triple) : Int32 :=
  t.fst + t.snd + t.thd

def makePair (a b : UInt32) : UInt32 × UInt32 :=
  (a + 1, b + 2)

def sumPair (p : UInt32 × UInt32) : UInt32 :=
  p.1 + p.2

def swapPair (p : UInt32 × String) : String × UInt32 :=
  (p.2, p.1)

def roundTripPair (a b : UInt32) : UInt32 :=
  let p := makePair a b
  sumPair p

def nestedPair (a : UInt32) (b : Int32) (c : String) : UInt32 × (Int32 × String) :=
  (a, (b, c))

def readNestedPair (p : UInt32 × (Int32 × String)) : String :=
  if p.1 > 0 then p.2.2 else ""

