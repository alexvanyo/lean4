import Lean.Compiler.Kotlin

/-!
Tests enum-like inductive data types and pattern matching (when expressions) in the Kotlin backend.
-/

set_option linter.unusedVariables false

inductive Color where
  | red
  | green
  | blue

def colorToCode (c : Color) : UInt32 :=
  match c with
  | .red => 1
  | .green => 2
  | .blue => 3

def compareValues (a b : UInt32) : Ordering :=
  if a < b then
    .lt
  else if a == b then
    .eq
  else
    .gt
