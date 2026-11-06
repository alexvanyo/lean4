import Lean.Compiler.Kotlin

/-!
Tests that calls to a `@[kotlin_types]` declaration cast arguments to its overridden parameter types:
a recursive call passes an erased function parameter unchanged, and a caller with a typed function
casts it to the erased type.
-/

namespace KotlinTypesCalls

@[kotlin_class "class StepState"]
structure StepState where
  x : Int32
  y : Int32
  gens : Nat

@[kotlin_types "UInt" "Any?" "(Any?) -> Any?" "StepState"]
def stepN (step : UInt32) (st : StepState) (f : StepState → StepState) : StepState :=
  if h : step = 0 then st
  else
    have : (step - 1).toNat < step.toNat := by grind
    stepN (step - 1) (f st) f
termination_by step.toNat

def stepWith (step : UInt32) (st : StepState) (f : StepState → StepState) : StepState :=
  stepN step st f

def advance (step : UInt32) (st : StepState) : StepState :=
  stepWith step st fun s => { s with x := s.x + 1, gens := s.gens + 1 }

end KotlinTypesCalls
