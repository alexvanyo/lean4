import Lean.Compiler.Kotlin

/-!
Tests that passing an array or mutable class to a concurrent task deeply escapes
the value, preventing subsequent in-place updates.
-/

set_option linter.unusedVariables false

def testConcurrentArray (a : Array Nat) : IO (Array Nat) := do
  let t ← BaseIO.asTask (pure a)
  let a' := a.set! 0 42
  pure a'
