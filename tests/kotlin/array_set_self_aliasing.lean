import Lean.Compiler.Kotlin

/-!
Tests that passing an in-place update target that aliases another argument
at the same call site fails static linear ownership verification.
-/

set_option linter.unusedVariables false

structure Box where
  arr : Array Nat

@[extern "kotlin_inplace:{0}[0] = 42"]
opaque inplaceOp (a : Array Nat) (b : Box) : Array Nat

private def testInplaceOverlap (a : Array Nat) : Array Nat :=
  let b : Box := { arr := a }
  inplaceOp a b

