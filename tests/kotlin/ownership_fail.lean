import Lean.Compiler.Kotlin

/-!
Tests that constructing a @[kotlin_class] instance from scratch without updating
an existing owned instance fails static linear ownership verification.
-/

set_option linter.unusedVariables false

@[kotlin_class "Buffer"]
structure Buffer where
  size : Int32
  capacity : Int32

@[kotlin_member "Buffer" "public" "badMake"]
def Buffer.badMake (b : Buffer) (s : Int32) : Buffer :=
  { size := s, capacity := 10 }
