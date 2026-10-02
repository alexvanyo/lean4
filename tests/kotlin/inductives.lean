import Lean.Compiler.Kotlin

/-!
Tests enum-like and payload-carrying inductive data types, standard ADTs (`Option`, `List`,
`Except`), and non-`@[kotlin_class]` structures in the Kotlin backend.
-/

set_option linter.unusedVariables false

inductive Color where
  | red
  | green
  | blue

def colorToCode (c : Color) : UInt32 :=
  match c with
  | .red => 1
  | .green => 2
  | .blue => 3

def compareValues (a b : UInt32) : Ordering :=
  if a < b then
    .lt
  else if a == b then
    .eq
  else
    .gt

def optionGetD (o : Option UInt32) (d : UInt32) : UInt32 :=
  match o with
  | .none => d
  | .some x => x

def optionMapInc (o : Option UInt32) : Option UInt32 :=
  match o with
  | .none => .none
  | .some x => .some (x + 1)

def testOption (x : UInt32) : UInt32 :=
  optionGetD (optionMapInc (.some x)) 0 + optionGetD (optionMapInc .none) 10

def sumList (xs : List UInt32) : UInt32 :=
  match xs with
  | [] => 0
  | x :: rest => x + sumList rest

def makeList3 (a b c : UInt32) : List UInt32 :=
  [a, b, c]

def makeExcept (ok : Bool) (v : UInt32) : Except String UInt32 :=
  if ok then .ok v else .error "failed"

def exceptToCode (e : Except String UInt32) : UInt32 :=
  match e with
  | .ok v => v
  | .error _ => 999

structure BoxedItem where
  tag : String
  count : UInt32
  weight : Float

def mkBoxedItem (tag : String) (count : UInt32) (weight : Float) : BoxedItem :=
  { tag, count, weight }

def bumpBoxedItem (item : BoxedItem) (delta : UInt32) : BoxedItem :=
  { item with count := item.count + delta }

def boxedItemTag (item : BoxedItem) : String :=
  item.tag

def boxedItemCount (item : BoxedItem) : UInt32 :=
  item.count

def boxedItemWeight (item : BoxedItem) : Float :=
  item.weight

inductive ExprTree where
  | lit (v : Int32)
  | add (l r : ExprTree)
  | neg (e : ExprTree)

def evalTree (t : ExprTree) : Int32 :=
  match t with
  | .lit v => v
  | .add l r => evalTree l + evalTree r
  | .neg e => - evalTree e

def sampleTree (x y : Int32) : ExprTree :=
  .add (.lit x) (.neg (.lit y))

