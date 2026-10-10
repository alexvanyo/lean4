import Lean.Compiler.Kotlin

/-!
Tests that passing an aliased array through an inlined loop back-edge fails
exclusive ownership verification on the back edge.
-/

set_option linter.unusedVariables false

@[inline]
partial def loopArray (n : Nat) (i : Nat) (a : Array Nat) (acc : List (Array Nat)) : Array Nat :=
  if i < n then
    let a' := a.set! 0 42
    loopArray n (i + 1) a' (a' :: acc)
  else
    a

def testInlinedLoopAliasing (n : Nat) (a : Array Nat) : Array Nat :=
  loopArray n 0 a []
