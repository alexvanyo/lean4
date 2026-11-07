import Lean.Compiler.Kotlin

/-!
Tests `@[kotlin_types]` overrides of type `Array<T>`, which differ at runtime from the `Array<Any?>`
of Lean arrays: results and `@[kotlin_class]` fields are converted rather than cast, results are not
dropped, and parameters are copied before they are updated in place.
-/

namespace TypedArrayOverrides

@[kotlin_types "Array<Pair<UInt, UInt>>" "Array<String>"]
def labels (xs : Array (UInt32 × UInt32)) : Array String :=
  xs.map fun p => s!"{p.1},{p.2}"

@[kotlin_types "_" "Array<String>"]
def names (n : Nat) : Array String :=
  (List.range n).toArray.map toString

@[kotlin_class "class Named", kotlin_types "_" "Array<String>"]
structure Named where
  count : UInt32
  items : Array String

def mkNamed (n : Nat) : Named := ⟨n.toUInt32, names n⟩

end TypedArrayOverrides
