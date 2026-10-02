import Lean.Compiler.Kotlin

/-!
Tests tail-recursive loops with scalar accumulators in the Kotlin backend.
Adapted from `tests/compile/uint_fold.lean`.
-/

set_option linter.unusedVariables false

partial def countUp (x y : UInt32) : UInt32 :=
  if x == 0 then y else countUp (x - 1) (y + 2)

partial def foldBits (n : UInt32) (acc : UInt32) : UInt32 :=
  if n == 0 then
    acc
  else
    foldBits (n >>> 1) ((acc <<< 1) ||| (n &&& 1))
