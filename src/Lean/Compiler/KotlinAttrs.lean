/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Lean FRO, LLC
-/
module

prelude
public import Lean.Attributes
public import Lean.Parser.Attr
public import Lean.MonadEnv
public import Lean.Structure

public section

namespace Lean.Parser.Attr

/-- `@[kotlin_member "RecvType" "modifiers"? "name"?]`: see `Lean.Compiler.kotlinMemberAttr`. -/
@[builtin_attr_parser] def kotlin_member := leading_parser
  nonReservedSymbol "kotlin_member" >> many1 (ppSpace >> strLit)

end Lean.Parser.Attr

namespace Lean.Compiler

/-- Parameters of `@[kotlin_member]`. -/
structure KotlinMemberInfo where
  /-- Kotlin type of the receiver, e.g. `"Foo<*, *>"`. The class name is the text before `<`. -/
  recvType : String
  /-- Kotlin modifiers placed before `fun` (the backend adds `inline`/`tailrec` itself). -/
  modifiers : String := "internal"
  /-- Kotlin member name; defaults to the last component of the declaration name. -/
  name? : Option String := none
  deriving Inhabited

/--
`@[kotlin_member "RecvType" "modifiers"? "name"?]` makes the Kotlin backend emit the declaration
as a member function of the Kotlin class named by `RecvType`. The first explicit parameter becomes
`this`. Members are spliced into the Kotlin preamble/footer at a line
`// @LeanMembers(ClassName)`.
-/
builtin_initialize kotlinMemberAttr : ParametricAttribute KotlinMemberInfo ←
  registerParametricAttribute {
    name := `kotlin_member
    descr := "emit this declaration as a member of a Kotlin class (Kotlin backend)"
    getParam := fun _ stx => do
      let strs := stx[1].getArgs.filterMap (·.isStrLit?)
      match strs.toList with
      | [recv] => return { recvType := recv }
      | [recv, mods] => return { recvType := recv, modifiers := mods }
      | [recv, mods, n] => return { recvType := recv, modifiers := mods, name? := some n }
      | _ => throwError "`kotlin_member` expects 1 to 3 string arguments: receiver type, modifiers, name"
  }

def getKotlinMemberInfo? (env : Environment) (n : Name) : Option KotlinMemberInfo :=
  kotlinMemberAttr.getParam? env n

/-- Kotlin class name of a receiver type string (`"Foo<*, *>"` ↦ `"Foo"`). -/
def KotlinMemberInfo.className (info : KotlinMemberInfo) : String :=
  String.ofList (info.recvType.toList.takeWhile (· != '<') |>.filter (· != ' '))

end Lean.Compiler

namespace Lean.Parser.Attr

/-- `@[kotlin_types "T₁" … "Tₙ" "R"]`: see `Lean.Compiler.kotlinTypesAttr`. -/
@[builtin_attr_parser] def kotlin_types := leading_parser
  nonReservedSymbol "kotlin_types" >> many1 (ppSpace >> strLit)

end Lean.Parser.Attr

namespace Lean.Compiler

/--
`@[kotlin_types "T₁" … "Tₙ" "R"]` overrides the Kotlin types the Kotlin backend uses in the
declaration's signature: `Tᵢ` for the `i`-th emitted parameter (the receiver of a
`@[kotlin_member]` is not counted) and `R` for the return type. `"_"` keeps the default type.
Returned values are cast to `R`, which allows using type parameters of the enclosing Kotlin class.
-/
builtin_initialize kotlinTypesAttr : ParametricAttribute (Array String) ←
  registerParametricAttribute {
    name := `kotlin_types
    descr := "override Kotlin parameter/return types in the signature (Kotlin backend)"
    getParam := fun _ stx => do
      return stx[1].getArgs.filterMap (·.isStrLit?)
  }

def getKotlinTypes? (env : Environment) (n : Name) : Option (Array String) :=
  kotlinTypesAttr.getParam? env n

/--
`@[kotlin_file]` marks a constant of type `Lean.Compiler.Kotlin.FileSpec` describing the layout
of the Kotlin file the Kotlin backend emits for this module (classes, fields, verbatim code).
-/
builtin_initialize kotlinFileAttr : TagAttribute ←
  registerTagAttribute `kotlin_file "Kotlin file layout for this module (Kotlin backend)"

end Lean.Compiler

namespace Lean.Parser.Attr

/-- `@[kotlin_class "Type"]`: see `Lean.Compiler.kotlinClassAttr`. -/
@[builtin_attr_parser] def kotlin_class := leading_parser
  nonReservedSymbol "kotlin_class" >> ppSpace >> strLit

/-- `@[mutable_kotlin_class "Type"]`: see `Lean.Compiler.mutableKotlinClassAttr`. -/
@[builtin_attr_parser] def mutable_kotlin_class := leading_parser
  nonReservedSymbol "mutable_kotlin_class" >> ppSpace >> strLit

end Lean.Parser.Attr

namespace Lean.Compiler

/-- Parsed specification of a `@[kotlin_class]` attribute. -/
structure KotlinClassSpec where
  /-- Raw string argument from attribute, e.g. `"data sealed interface Shape"`. -/
  raw : String
  /-- Extracted Kotlin type/class name (e.g. `"Shape"` or `"Foo<*, *>"`). -/
  className : String
  /-- Whether `data` modifier was specified. -/
  isData : Bool := false
  /-- Explicit kind override: `"enum class"`, `"sealed interface"`, `"sealed class"`, `"class"`, or none. -/
  kind? : Option String := none
  deriving Inhabited

/-- Splits a string into words separated by spaces. -/
def splitWords (s : String) : List String :=
  let (acc, cur) := s.foldl (init := (([] : List String), "")) fun (acc, cur) c =>
    if c == ' ' then (if cur.isEmpty then acc else cur :: acc, "") else (acc, cur.push c)
  (if cur.isEmpty then acc else cur :: acc).reverse

/-- Parses a `@[kotlin_class]` string argument into modifiers and class name. -/
def parseKotlinClassSpec (raw : String) : KotlinClassSpec := Id.run do
  let tokens := splitWords raw
  let mut isData := false
  let mut kindTokens : List String := []
  let mut remTokens : List String := tokens
  if remTokens.head? == some "data" then
    isData := true
    remTokens := remTokens.tail
  if remTokens.take 2 == ["sealed", "interface"] then
    kindTokens := ["sealed", "interface"]
    remTokens := remTokens.drop 2
  else if remTokens.take 2 == ["sealed", "class"] then
    kindTokens := ["sealed", "class"]
    remTokens := remTokens.drop 2
  else if remTokens.take 2 == ["enum", "class"] then
    kindTokens := ["enum", "class"]
    remTokens := remTokens.drop 2
  else if remTokens.head? == some "class" then
    kindTokens := ["class"]
    remTokens := remTokens.tail
  let kind? := if kindTokens.isEmpty then none else some (String.intercalate " " kindTokens)
  let clsName := String.intercalate " " remTokens
  { raw, className := clsName, isData, kind? }

/-- Base Kotlin class name without type arguments (`"Foo<*, *>"` ↦ `"Foo"`). -/
def KotlinClassSpec.baseClassName (spec : KotlinClassSpec) : String :=
  String.ofList (spec.className.toList.takeWhile (· != '<') |>.filter (· != ' '))

/--
`@[kotlin_class "Type"]` on a structure or inductive type makes the Kotlin backend represent
values as instances of a Kotlin class, sealed hierarchy, or enum class.
-/
builtin_initialize kotlinClassAttr : ParametricAttribute String ←
  registerParametricAttribute {
    name := `kotlin_class
    descr := "represent this structure or inductive type as a Kotlin class/interface/enum (Kotlin backend)"
    getParam := fun declName stx => do
      let some s := stx[1].isStrLit? | throwError "`kotlin_class` expects a string argument"
      let env ← getEnv
      unless isStructure env declName || isInductiveCore env declName do
        throwError "`kotlin_class` can only be used on structures or inductive types"
      let spec := parseKotlinClassSpec s
      if isInductiveCore env declName && !isStructure env declName then
        if spec.kind? == some "enum class" then
          let some iv := isInductiveCore? env declName | unreachable!
          for ctorName in iv.ctors do
            let some (.ctorInfo cv) := env.find? ctorName | unreachable!
            if cv.numFields > 0 then
              throwError "`enum class` can only be used on inductive types whose constructors take no arguments (constructor `{ctorName}` has {cv.numFields} field(s))"
        else if spec.kind? == some "class" then
          throwError "`class` without `sealed` can only be used on structures"
      return s
  }

/--
`@[mutable_kotlin_class "Type"]` on a structure makes the Kotlin backend represent values of the
structure as instances of the mutable Kotlin class `Type` (e.g. `"Foo<*, *>"`), whose properties
have the names of the structure fields. Values are mutated in place: the backend verifies statically
that every value it updates is exclusively owned, and rejects the program otherwise.
-/
builtin_initialize mutableKotlinClassAttr : ParametricAttribute String ←
  registerParametricAttribute {
    name := `mutable_kotlin_class
    descr := "represent this structure as a mutable Kotlin class updated in place (Kotlin backend)"
    getParam := fun declName stx => do
      let some s := stx[1].isStrLit? | throwError "`mutable_kotlin_class` expects a string argument"
      unless isStructure (← getEnv) declName do
        throwError "`mutable_kotlin_class` can only be used on structures"
      return s
  }

def getKotlinClassSpec? (env : Environment) (n : Name) : Option KotlinClassSpec :=
  (kotlinClassAttr.getParam? env n).map parseKotlinClassSpec

def isKotlinInductive (env : Environment) (n : Name) : Bool :=
  isInductiveCore env n && !isStructure env n && (kotlinClassAttr.getParam? env n).isSome

def isKotlinEnum (env : Environment) (n : Name) : Bool :=
  match isInductiveCore? env n with
  | some iv =>
    if isStructure env n then false
    else
      match (getKotlinClassSpec? env n).bind (·.kind?) with
      | some "enum class" => true
      | some "sealed class" | some "sealed interface" | some "class" => false
      | _ =>
        !iv.ctors.isEmpty && iv.ctors.all fun ctorName =>
          match env.find? ctorName with
          | some (.ctorInfo cv) => cv.numFields == 0
          | _ => false
  | none => false

def getImmutableKotlinClass? (env : Environment) (n : Name) : Option String :=
  (getKotlinClassSpec? env n).map (·.className)

def getMutableKotlinClass? (env : Environment) (n : Name) : Option String :=
  mutableKotlinClassAttr.getParam? env n

def getKotlinClass? (env : Environment) (n : Name) : Option String :=
  (getKotlinClassSpec? env n).map (·.className) |>.orElse fun _ => mutableKotlinClassAttr.getParam? env n

def isMutableKotlinClass (env : Environment) (n : Name) : Bool :=
  (mutableKotlinClassAttr.getParam? env n).isSome

end Lean.Compiler
