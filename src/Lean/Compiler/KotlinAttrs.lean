/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Lean FRO, LLC
-/
module

prelude
public import Lean.Attributes
public import Lean.Parser.Attr
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

end Lean.Parser.Attr

namespace Lean.Compiler

/--
`@[kotlin_class "Type"]` on a structure makes the Kotlin backend represent values of the structure
as instances of the Kotlin class `Type` (e.g. `"Foo<*, *>"`), whose properties have the names of the
structure fields. Values are mutated in place: the backend verifies statically that every value
it updates is exclusively owned, and rejects the program otherwise.
-/
builtin_initialize kotlinClassAttr : ParametricAttribute String ←
  registerParametricAttribute {
    name := `kotlin_class
    descr := "represent this structure as a Kotlin class (Kotlin backend)"
    getParam := fun declName stx => do
      let some s := stx[1].isStrLit? | throwError "`kotlin_class` expects a string argument"
      unless isStructure (← getEnv) declName do
        throwError "`kotlin_class` can only be used on structures"
      return s
  }

def getKotlinClass? (env : Environment) (n : Name) : Option String :=
  kotlinClassAttr.getParam? env n

end Lean.Compiler
