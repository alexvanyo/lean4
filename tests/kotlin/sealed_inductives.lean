import Lean.Compiler.Kotlin

/-!
Tests generating Kotlin enum classes, sealed interfaces, and sealed classes from inductive types,
as well as data classes for structures.
Verifies PascalCase constructor/variant naming, optional `data` modifier, member functions,
typed constructor allocations, and exhaustive `when` pattern matching.
-/

set_option linter.unusedVariables false

-- 1. Enum class from 0-arg inductive type
@[kotlin_class "TrafficLight"]
inductive TrafficLight where
  | red
  | green
  | yellow

def lightToCode (c : TrafficLight) : UInt32 :=
  match c with
  | .red => 1
  | .green => 2
  | .yellow => 3

def makeLight (tag : UInt32) : TrafficLight :=
  if tag == 1 then .red
  else if tag == 2 then .green
  else .yellow

-- 2. Plain sealed interface with payload constructors
@[kotlin_class "ArithExpr"]
inductive ArithExpr where
  | lit (v : Int32)
  | add (l r : ArithExpr)
  | neg (e : ArithExpr)

def evalArith (t : ArithExpr) : Int32 :=
  match t with
  | .lit v => v
  | .add l r => evalArith l + evalArith r
  | .neg e => - evalArith e

def sampleArith (x y : Int32) : ArithExpr :=
  .add (.lit x) (.neg (.lit y))

@[kotlin_member "ArithExpr" "public" "eval"]
def ArithExpr.eval (t : ArithExpr) : Int32 :=
  evalArith t

-- 3. Data sealed interface with mixed 0-arg and payload constructors
@[kotlin_class "data sealed interface Shape"]
inductive Shape where
  | empty
  | circle (radius : Float)
  | rect (width : Float) (height : Float)

def area (s : Shape) : Float :=
  match s with
  | .empty => 0.0
  | .circle r => 3.141592653589793 * r * r
  | .rect w h => w * h

def makeEmpty : Shape := .empty
def makeCircle (r : Float) : Shape := .circle r
def makeRect (w h : Float) : Shape := .rect w h

-- 4. Data sealed class
@[kotlin_class "data sealed class JsonValue"]
inductive JsonValue where
  | nullVal
  | boolVal (b : Bool)
  | numVal (n : Int32)
  | strVal (s : String)

def jsonToString (j : JsonValue) : String :=
  match j with
  | .nullVal => "null"
  | .boolVal b => if b then "true" else "false"
  | .numVal n => s!"{n}"
  | .strVal s => s!"\"{s}\""

def makeJsonNull : JsonValue := .nullVal
def makeJsonBool (b : Bool) : JsonValue := .boolVal b
def makeJsonNum (n : Int32) : JsonValue := .numVal n
def makeJsonStr (s : String) : JsonValue := .strVal s

-- 5. Data class for structure
@[kotlin_class "data class Vec2"]
structure Vec2 where
  x : Int32
  y : Int32

def addVec2 (a b : Vec2) : Vec2 :=
  { x := a.x + b.x, y := a.y + b.y }
