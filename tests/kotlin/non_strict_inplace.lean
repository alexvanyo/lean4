import Lean.Compiler.Kotlin

/-!
Tests that when compiler.kotlin.strictInplace = false, unprovable array mutations
fall back to safe defensive copying (.copyOf()) instead of failing compilation.
-/

set_option linter.unusedVariables false

def testNonStrictArray (a : Array Nat) : Array Nat × Array Nat :=
  let a' := a.set! 0 42
  (a, a')
