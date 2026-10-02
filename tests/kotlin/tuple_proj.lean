import Lean.Compiler.Kotlin

/-!
Tests structure field projections in the Kotlin backend.
Adapted from `tests/compile/tuple.lean` for @[kotlin_class] field property accesses.
-/

set_option linter.unusedVariables false

@[kotlin_class "Triple"]
structure Triple where
  fst : Int32
  snd : Int32
  thd : Int32

@[kotlin_member "Triple" "public" "sumFields"]
def Triple.sumFields (t : Triple) : Int32 :=
  t.fst + t.snd + t.thd
