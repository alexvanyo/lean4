import Lean.Compiler.Kotlin

/-!
Tests that passing an exclusive argument that aliases a sub-object of another argument
at the same call site fails linear ownership verification.
-/

set_option linter.unusedVariables false

def mutateFirst (a : Array Nat) (p : Array Nat × Nat) : Array Nat × Nat :=
  let x := p.2
  let a' := a.set! 0 42
  (a', x)

def testSubobjectAliasing (p : Array Nat × Nat) : Array Nat × Nat :=
  let a := p.1
  mutateFirst a p
