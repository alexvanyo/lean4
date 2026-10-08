/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Lean FRO, LLC
-/
module

prelude
public import Init.Data.Array.Basic

public section

/-!
Data describing the structure of a Kotlin source file emitted by the Kotlin backend.

A module provides a value of type `Lean.Compiler.Kotlin.FileSpec` tagged with `@[kotlin_file]`.
The backend evaluates it and emits the described file, placing the module's `@[kotlin_member]`
declarations inside the corresponding classes and the remaining declarations at top level.
-/

namespace Lean.Compiler.Kotlin

/-- An item in the body of a Kotlin class, emitted in order. -/
inductive ClassItem where
  /-- A property declaration, e.g. `"@JvmField internal var size: Int = 0"`. -/
  | field (decl : String)
  /-- An `init { ... }` block with the given body. -/
  | init (body : String)
  /-- All `@[kotlin_member]` declarations of this class, in declaration order. -/
  | members
  /-- Kotlin source emitted as is (re-indented to the class body). -/
  | verbatim (code : String)

instance : Inhabited ClassItem := ⟨.members⟩

/-- A Kotlin class. -/
structure ClassSpec where
  /-- Class name, matched against the receiver type of `@[kotlin_member]` declarations. -/
  name : String
  /-- Everything before the opening brace, e.g. `"public class Foo<K> : Bar()"`, including KDoc. -/
  header : String
  body : Array ClassItem := #[.members]

instance : Inhabited ClassSpec := ⟨{ name := "", header := "" }⟩

/-- A top-level item of a Kotlin file, emitted in order. -/
inductive FileItem where
  | cls (spec : ClassSpec)
  /-- Synthesized classes from `@[kotlin_class]` that are not explicitly specified as `.cls`. -/
  | classes
  /-- All top-level (non-member) declarations emitted by the backend. -/
  | topLevel
  /-- Kotlin source emitted as is. -/
  | verbatim (code : String)

instance : Inhabited FileItem := ⟨.topLevel⟩

/-- A Kotlin source file. The backend adds its own `@file:Suppress` annotation and the package. -/
structure FileSpec where
  /-- Emitted first, e.g. a license comment. -/
  preamble : String := ""
  /-- Additional file annotations, e.g. `"@file:JvmName(\"Foo\")"`. -/
  fileAnnotations : Array String := #[]
  /-- Import lines. -/
  imports : Array String := #[]
  items : Array FileItem := #[.topLevel]

instance : Inhabited FileSpec := ⟨{}⟩

end Lean.Compiler.Kotlin
