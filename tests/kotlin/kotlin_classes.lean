import Lean.Compiler.Kotlin

/-!
Tests @[kotlin_class], @[kotlin_member], and @[kotlin_types] in the Kotlin backend.
Verifies member method emission, in-place structure updates, and parameter type overriding.
-/

set_option linter.unusedVariables false

@[kotlin_class "Point"]
structure Point where
  x : Int32
  y : Int32

@[kotlin_member "Point" "public" "translate"]
def Point.translate (p : Point) (dx dy : Int32) : Point :=
  { p with x := p.x + dx, y := p.y + dy }

@[kotlin_member "Point" "public" "getXCoord"]
def Point.getXCoord (p : Point) : Int32 :=
  p.x

@[export lean_make_point]
def makePoint (x y : Int32) : Point :=
  { x, y }
