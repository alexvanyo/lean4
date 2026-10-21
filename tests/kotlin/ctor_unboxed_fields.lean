import Lean.Compiler.Kotlin

/-!
Tests that constructors with scalar fields read from boxed elements (of a list here) get every
field. The IR unboxes such an element after allocating the object and before the field writes
that initialize it.
-/

set_option linter.unusedVariables false

@[kotlin_class "sealed class Frame"]
inductive Frame where
  | node (id : UInt32) (x : Int64) (fuel : UInt32)
  | leaf (bits : UInt64) (x : Int64)

@[export lean_frame_children]
def frameChildren (ids : List UInt32) (bits : List UInt64) (x half : Int64) (fuel : UInt32) : List Frame :=
  [.node (ids.getD 0 0) x (fuel - 1), .node (ids.getD 1 0) (x + half) (fuel - 1), .leaf (bits.getD 0 0) (x + half)]

inductive Cell where
  | live (id : UInt32) (x : Int64)
  | dead

@[export lean_cell_children]
def cellChildren (ids : List UInt32) (x half : Int64) : List Cell :=
  [.live (ids.getD 0 0) x, .live (ids.getD 1 0) (x + half), .dead]

@[export lean_cell_id]
def cellId : Cell → UInt32
  | .live id _ => id
  | .dead => 0

@[export lean_cell_x]
def cellX : Cell → Int64
  | .live _ x => x
  | .dead => -1
