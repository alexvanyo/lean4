import Lean.Compiler.Kotlin

/-!
Tests that a loop back-edge occurring in a branch before an in-place update
cannot pass a shared array to bypass exclusive ownership verification.
-/

set_option linter.unusedVariables false

private partial def loopBranch (n : Nat) (i : Nat) (a : Array Nat) (shared : Array Nat) : Array Nat :=
  if i < n then
    if i == 0 then
      loopBranch n (i + 1) shared shared
    else
      let a' := a.set! 0 42
      loopBranch n (i + 1) a' shared
  else
    a

private def testLoopBranchOrder (n : Nat) (a : Array Nat) (shared : Array Nat) : Array Nat :=
  loopBranch n 0 a shared
