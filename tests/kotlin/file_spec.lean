import Lean.Compiler.Kotlin

/-!
Tests @[kotlin_file] FileSpec layout DSL in the Kotlin backend.
Verifies class layout, fields, init blocks, member injection, and file-level annotations.
-/

open Lean.Compiler.Kotlin

@[kotlin_file]
def myFileSpec : FileSpec := {
  fileAnnotations := #["@file:JvmName(\"MyGeneratedFile\")"]
  imports := #["import java.util.concurrent.atomic.AtomicInteger"]
  items := #[
    .cls {
      name := "MyCounter"
      header := "public class MyCounter(initial: Int)"
      body := #[
        .field "private var current: Int = initial",
        .init "current = initial",
        .members,
        .verbatim "fun reset() { current = 0 }"
      ]
    },
    .topLevel
  ]
}

@[kotlin_class "MyCounter"]
structure MyCounter where
  current : Int32

@[kotlin_member "MyCounter" "public" "add"]
def MyCounter.add (c : MyCounter) (delta : Int32) : MyCounter :=
  { c with current := c.current + delta }

def helperTopLevel (x : Int32) : Int32 :=
  x * 2
