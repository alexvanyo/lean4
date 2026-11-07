import Lean.Compiler.Kotlin

/-!
Tests values whose signedness is changed by an erased conversion such as `UInt32.toInt32`: a boxed
`Int32` pair component is a Kotlin `Int`, and widening an `Int32` reinterpreted as `UInt32` does not
sign-extend.
-/

namespace ErasedSignedness

@[noinline] def pushCell (cells : Array (Int32 × Int32)) (col row : UInt32) : Array (Int32 × Int32) :=
  cells.push (col.toInt32, row.toInt32)

def mkCell (col row : UInt32) : Int32 × Int32 :=
  let x := col.toInt32
  let y := row.toInt32
  (x, y)

def widenSnd (c : Int32 × Int32) : UInt64 := c.2.toUInt32.toUInt64

def widen (c : Int32) : UInt64 := c.toUInt32.toUInt64 + 1

def narrowToNat (c : UInt8) : Int64 := c.toInt8.toInt64

end ErasedSignedness
