import Lean.Compiler.Kotlin

/-!
Tests `Array.append` on generic arrays and reads of `@[kotlin_class]` elements from generic arrays,
which must be cast from `Any?`.
-/

namespace GenericArrayElems

/-- `+` would be ambiguous between `plus(element)` and `plus(elements)` on `Array<Any?>`. -/
def appendArrays (a b : Array String) : Array String := a ++ b

structure Buf (α : Type) where
  data : Array α
  size : UInt32

@[kotlin_class "ElemItem"]
structure ElemItem where
  a : UInt32
  b : String
  deriving Inhabited

def itemA (i : ElemItem) : UInt32 := i.a

def firstA (b : Buf ElemItem) : UInt32 := itemA b.data[0]!

@[noinline] def itemAt (items : Array ElemItem) (i : UInt32) : ElemItem := items[i.toNat]!

def itemAtB (items : Array ElemItem) (i : UInt32) : String := (itemAt items i).b

end GenericArrayElems
