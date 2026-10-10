import Lean.Compiler.Kotlin

/-!
Tests that returning a structure containing a shared parameter cannot be claimed
as fresh by the caller to mutate it in place.
-/

set_option linter.unusedVariables false

structure Container where
  inner : Array Nat × Nat
  tag : Nat

private def makeContainer (shared : Array Nat) : Container :=
  let inner := (shared, 0)
  ⟨inner, 1⟩

def testReturnFreshSoundness (shared : Array Nat) : Array Nat :=
  let res := makeContainer shared
  let inner := res.inner
  let a := inner.1
  a.set! 0 999
