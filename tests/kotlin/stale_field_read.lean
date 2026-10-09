import Lean.Compiler.Kotlin

/-!
Tests that mutating a mutable class in-place forces re-reading its fields,
preventing stale field reads in the Kotlin backend.
-/

set_option linter.unusedVariables false

@[mutable_kotlin_class "StaleCounter"]
structure StaleCounter where
  count : Int32

@[kotlin_member "StaleCounter" "public" "bump"]
def StaleCounter.bump (c : StaleCounter) : StaleCounter :=
  { c with count := c.count + 1 }

@[kotlin_member "StaleCounter" "public" "testRead"]
def StaleCounter.testRead (c : StaleCounter) : Int32 × StaleCounter :=
  let x := c.count
  let c1 := c.bump
  let y := c1.count
  (x + y, c1)
