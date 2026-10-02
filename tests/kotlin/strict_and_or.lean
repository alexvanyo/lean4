import Lean.Compiler.Kotlin

/-!
Tests strict boolean evaluation, logic operations, and branching in the Kotlin backend.
Adapted from `tests/compile/strictAndOr.lean`.
-/

set_option linter.unusedVariables false

def myStrictAnd (a b : Bool) : Bool :=
  a && b

def myStrictOr (a b : Bool) : Bool :=
  a || b

def condBranch (x y : UInt32) : UInt32 :=
  if x > y && y > 0 then
    x - y
  else if x == y || x == 0 then
    x + y
  else
    0
