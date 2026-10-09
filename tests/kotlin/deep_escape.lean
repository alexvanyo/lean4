import Lean.Compiler.Kotlin

/-!
Tests that passing an aggregate object to an escaping consumer deeply escapes
all reachable children, preventing subsequent in-place updates.
-/

set_option linter.unusedVariables false

@[extern "consume_pair"]
opaque consumePair (p : Prod (Array Nat) Nat) : IO Unit

def testDeepEscape (p : Prod (Array Nat) Nat) : IO (Array Nat) := do
  let a := p.1
  consumePair p
  let a' := a.set! 0 42
  pure a'
