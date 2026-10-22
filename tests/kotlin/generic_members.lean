import Lean.Compiler.Kotlin

/-!
Tests declarations with a type parameter after a runtime parameter, such as the receiver of a member: the type
parameter takes no Kotlin argument at definitions, calls and partial applications, the member keeps its receiver
(and returns it implicitly), and a dropped `Prod` result component of the type parameter's type is declared with it.
These members return several values without allocating, through an inline continuation.
-/

set_option linter.unusedVariables false

@[mutable_kotlin_class "GenericCounter"]
structure GenericCounter where
  count : UInt32
  total : UInt64

@[inline]
def GenericCounter.bump (c : GenericCounter) (n : UInt32) : (UInt32 × UInt32) × GenericCounter :=
  ((c.count, c.count + n), { c with count := c.count + n, total := c.total + n.toUInt64 })

/-- The type parameter before the receiver. -/
@[inline, kotlin_member "GenericCounter" "public" "bumpBefore",
  kotlin_contract "callsInPlace(k, kotlin.contracts.InvocationKind.EXACTLY_ONCE)"]
def GenericCounter.bumpBefore {β : Type} (c : GenericCounter) (n : UInt32) (k : UInt32 → UInt32 → β) :
    β × GenericCounter :=
  let ((a, b), c') := c.bump n
  (k a b, c')

/-- The type parameter after the receiver. -/
@[inline, kotlin_member "GenericCounter" "public" "bumpAfter",
  kotlin_contract "callsInPlace(k, kotlin.contracts.InvocationKind.EXACTLY_ONCE)"]
def GenericCounter.bumpAfter (c : GenericCounter) {β : Type} (n : UInt32) (k : UInt32 → UInt32 → β) :
    β × GenericCounter :=
  let ((a, b), c') := c.bump n
  (k a b, c')

/-- A type parameter after a runtime parameter of a top-level function. -/
@[noinline]
def applyAfter (n : UInt32) {α : Type} (f : UInt32 → α) : α := f (n + 1)

@[export lean_apply_after_twice]
def applyAfterTwice (n : UInt32) : UInt64 :=
  applyAfter n (fun x => x.toUInt64) + applyAfter (n + 1) (fun x => x.toUInt64 * 10)

@[export lean_apply_after_each]
def applyAfterEach (fs : List (UInt32 → UInt64)) : List UInt64 := fs.map (applyAfter 3)
