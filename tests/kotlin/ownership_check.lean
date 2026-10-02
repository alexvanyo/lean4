import Lean.Compiler.Kotlin

/-!
Tests KotlinOwnership analysis for exclusive structure ownership, ensuring safe
in-place updates and redundant parameter identity elimination (dropShape?).
-/

set_option linter.unusedVariables false

@[mutable_kotlin_class "Buffer"]
structure Buffer where
  size : Int32
  capacity : Int32

@[kotlin_member "Buffer" "public" "clear"]
def Buffer.clear (b : Buffer) : Buffer :=
  { b with size := 0 }

@[kotlin_member "Buffer" "public" "grow"]
def Buffer.grow (b : Buffer) (additional : Int32) : Buffer :=
  { b with capacity := b.capacity + additional }
