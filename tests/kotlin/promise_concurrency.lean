import Lean.Compiler.Kotlin

/-!
Tests that resolving an IO.Promise with an array deeply escapes it,
preventing subsequent in-place updates.
-/

set_option linter.unusedVariables false

def testPromiseEscape (a : Array Nat) : IO (Array Nat) := do
  let p ← IO.Promise.new
  p.resolve a
  let a' := a.set! 0 42
  pure a'
