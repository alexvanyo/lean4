import Lean.Compiler.Kotlin

/-!
Tests that an exported (`@[export]`) function that requires exclusive
parameter ownership is rejected as an external entry point.
-/

set_option linter.unusedVariables false

@[export publicMutate]
def publicMutate (a : Array Nat) : Array Nat :=
  a.set! 0 42
