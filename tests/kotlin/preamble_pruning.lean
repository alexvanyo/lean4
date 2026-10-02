import Lean.Compiler.Kotlin

/-!
Tests custom package, preamble inclusion, and dead code pruning (compiler.kotlin.pruneUnreachable).
Verifies that unreachable functions are omitted while exported roots and their dependencies remain.
-/

set_option linter.unusedVariables false

def unreachableHelper (x : UInt32) : UInt32 :=
  x * 100

def reachableHelper (x : UInt32) : UInt32 :=
  x + 42

@[export lean_exported_add]
def exportedAdd (a b : UInt32) : UInt32 :=
  reachableHelper (a + b)
