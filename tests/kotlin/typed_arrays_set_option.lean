import Lean.Compiler.Kotlin

/-!
Tests `compiler.kotlin.typedArrays` set in the file rather than on the command line: signatures and
`@[kotlin_class]` fields use the typed arrays of the compiled bodies, and empty arrays of different
element types are not shared.
-/

set_option compiler.kotlin.typedArrays true

namespace TypedArraysSetOption

@[kotlin_class "class CellBatch"]
structure CellBatch where
  warnings : Array String
  cells : Array UInt64

def sumCells (cells : Array UInt64) : UInt64 := cells.foldl (· + ·) 0

def mkBatch (n : UInt64) : CellBatch := ⟨#[], #[n, n + 1]⟩

def emptyBatch : CellBatch := ⟨#[], #[]⟩

def emptyBatchOf (n : UInt32) : CellBatch × UInt32 := (⟨#[], #[]⟩, n)

def total (b : CellBatch) : UInt64 := sumCells b.cells

end TypedArraysSetOption
