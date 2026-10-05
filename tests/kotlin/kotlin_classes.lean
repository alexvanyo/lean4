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

-- Standalone @[kotlin_class] with mixed object (String, Nat) and scalar (Int32) fields (synthesized without preamble)
@[kotlin_class "Person"]
structure Person where
  name : String
  age : Nat
  score : Int32

@[kotlin_member "Person" "public" "withScore"]
def Person.withScore (p : Person) (newScore : Int32) : Person :=
  { p with score := newScore }

@[kotlin_member "Person" "public" "birthday"]
def Person.birthday (p : Person) : Person :=
  { p with age := p.age + 1 }

@[kotlin_member "Person" "public" "summary"]
def Person.summary (p : Person) : String :=
  s!"{p.name}:{p.age}:{p.score}"

def makePerson (name : String) (age : Nat) (score : Int32) : Person :=
  { name, age, score }

-- Single-field immutable @[kotlin_class] unboxed to its underlying scalar type (Int)
@[kotlin_class "Meters"]
structure Meters where
  value : Int32

def addMeters (a b : Meters) : Meters :=
  { value := a.value + b.value }

def scaleMeters (m : Meters) (factor : Int32) : Meters :=
  { value := m.value * factor }

-- Single-field @[mutable_kotlin_class] kept boxed as `class Counter(var count: Int)` for in-place mutation
@[mutable_kotlin_class "Counter"]
structure Counter where
  count : Int32

@[kotlin_member "Counter" "public" "inc"]
def Counter.inc (c : Counter) (delta : Int32) : Counter :=
  { c with count := c.count + delta }

@[kotlin_member "Counter" "public" "readCount"]
def Counter.readCount (c : Counter) : Int32 :=
  c.count

-- Multi-field @[mutable_kotlin_class] with object fields (String, Nat) synthesized and updated in-place
@[mutable_kotlin_class "MutablePerson"]
structure MutablePerson where
  name : String
  age : Nat

@[kotlin_member "MutablePerson" "public" "birthday"]
def MutablePerson.birthday (p : MutablePerson) : MutablePerson :=
  { p with age := p.age + 1 }

@[kotlin_member "MutablePerson" "public" "rename"]
def MutablePerson.rename (p : MutablePerson) (newName : String) : MutablePerson :=
  { p with name := newName }

