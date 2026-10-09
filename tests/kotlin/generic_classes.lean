import Lean.Compiler.Kotlin
import Std.Data.HashMap
import Std.Data.HashSet
import Std.Data.TreeMap
import Std.Data.TreeSet

/-!
Tests generics support for generated Kotlin classes, sealed interfaces, and functions,
including generic collections (`Std.HashMap`, `Std.HashSet`, `Std.TreeMap`, `Std.TreeSet`).
Verifies type parameter mapping from Lean to Kotlin (<T>, <K, V>, <out T>),
typed constructors, member generic methods, pattern matching on generic ADTs,
and generic map/set data holders and operations.
-/

set_option linter.unusedVariables false

-- 1. Single-parameter generic structure
@[kotlin_class "Box"]
structure Box (α : Type) where
  val : α
  deriving BEq, Hashable

def makeBox (x : α) : Box α :=
  { val := x }

def unbox (b : Box α) : α :=
  b.val

def mapBox (f : α → β) (b : Box α) : Box β :=
  { val := f b.val }

@[noinline, kotlin_member "Box" "public" "extractVal"]
def Box.extractVal (b : Box α) : α :=
  b.val

@[kotlin_member "Box" "public" "mapVal"]
def Box.mapVal (b : Box α) (f : α → β) : Box β :=
  { val := f b.val }

-- 2. Multi-parameter generic structure with data class
@[kotlin_class "data class KeyValue"]
structure KeyValue (κ : Type) (υ : Type) where
  key : κ
  value : υ

def makeKeyValue (k : κ) (v : υ) : KeyValue κ υ :=
  { key := k, value := v }

def swapKeyValue (kv : KeyValue κ υ) : KeyValue υ κ :=
  { key := kv.value, value := kv.key }

-- 3. Generic sealed hierarchy
@[kotlin_class "data sealed interface CustomList"]
inductive CustomList (α : Type) where
  | nil
  | cons (head : α) (tail : CustomList α)

def makeCustomNil : CustomList α :=
  .nil

def makeCustomCons (h : α) (t : CustomList α) : CustomList α :=
  .cons h t

def customHeadD (xs : CustomList α) (defaultVal : α) : α :=
  match xs with
  | .nil => defaultVal
  | .cons h _ => h

-- 4. Nested generic structure
def wrapBox (x : α) : Box (Box α) :=
  { val := { val := x } }

-- 5. Generic class holding a HashMap
@[kotlin_class "data class GenericMapHolder"]
structure GenericMapHolder (α : Type) where
  entries : Std.HashMap String α

def makeGenericMapHolder (k : String) (v : α) : GenericMapHolder α :=
  let m : Std.HashMap String α := Std.HashMap.emptyWithCapacity 4
  { entries := m.insert k v }

def lookupGenericMap (holder : GenericMapHolder α) (k : String) (defaultVal : α) : α :=
  holder.entries.getD k defaultVal

@[kotlin_member "GenericMapHolder" "public" "lookup"]
def GenericMapHolder.lookup (holder : GenericMapHolder α) (k : String) (defaultVal : α) : α :=
  holder.entries.getD k defaultVal

-- 6. Generic class holding a TreeMap
@[kotlin_class "data class GenericTreeMapHolder"]
structure GenericTreeMapHolder (α : Type) where
  entries : Std.TreeMap String α

def makeGenericTreeMapHolder (k : String) (v : α) : GenericTreeMapHolder α :=
  let m : Std.TreeMap String α := {}
  { entries := m.insert k v }

def lookupGenericTreeMap (holder : GenericTreeMapHolder α) (k : String) (defaultVal : α) : α :=
  holder.entries.getD k defaultVal

-- 7. Generic map operations directly
def genericMapInsert (m : Std.HashMap String α) (k : String) (v : α) : Std.HashMap String α :=
  m.insert k v

def genericMapGet (m : Std.HashMap String α) (k : String) (defaultVal : α) : α :=
  m.getD k defaultVal

def makeEmptyMap : Std.HashMap String α :=
  Std.HashMap.emptyWithCapacity 4

-- 8. Maps containing generic Box and CustomList values
def mapWithBox (k : String) (v : α) : Std.HashMap String (Box α) :=
  let m : Std.HashMap String (Box α) := Std.HashMap.emptyWithCapacity 4
  m.insert k { val := v }

def getBoxValFromMap (m : Std.HashMap String (Box α)) (k : String) (defaultVal : α) : α :=
  match m.get? k with
  | some b => b.extractVal
  | none => defaultVal

def mapWithCustomList (k : String) (v : α) : Std.HashMap String (CustomList α) :=
  let m : Std.HashMap String (CustomList α) := Std.HashMap.emptyWithCapacity 4
  m.insert k (.cons v .nil)

def getHeadFromMap (m : Std.HashMap String (CustomList α)) (k : String) (defaultVal : α) : α :=
  match m.get? k with
  | some xs => customHeadD xs defaultVal
  | none => defaultVal

-- 9. Generic Set operations and sets of generic Box
def makeBoxSet (b1 b2 : Box String) : Std.HashSet (Box String) :=
  let s : Std.HashSet (Box String) := Std.HashSet.emptyWithCapacity 4
  let s := s.insert b1
  s.insert b2

def boxSetContains (s : Std.HashSet (Box String)) (query : Box String) : Bool :=
  s.contains query

def makeTreeSetWith (x : String) : Std.TreeSet String :=
  let s : Std.TreeSet String := {}
  s.insert x

def treeSetContains (s : Std.TreeSet String) (x : String) : Bool :=
  s.contains x

-- 10. Generic structure inside Option and field access on unwrapped value
@[kotlin_class "data class PairItem"]
structure PairItem (α : Type) where
  id : String
  val : α
deriving Inhabited

@[kotlin_class "data class Container"]
structure Container (α : Type) where
  optItem : Option (PairItem α)

def checkItem (c : Container α) (expectedId : String) : Bool :=
  match c.optItem with
  | none => false
  | some item => expectedId == item.id

-- Multi-branch join point with generic Option parameter
def stepContainer (c : Container α) (fallback : Option (PairItem α)) (cond1 cond2 : Bool) : Container α :=
  let nextOpt : Option (PairItem α) :=
    if cond1 then
      c.optItem
    else if cond2 then
      fallback
    else
      none
  { optItem := nextOpt }
