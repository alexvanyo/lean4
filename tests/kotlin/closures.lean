import Lean.Compiler.Kotlin

/-!
Tests higher-order functions, closures, currying, and partial applications in the Kotlin backend.
Adapted from .
-/

set_option linter.unusedVariables false

@[inline]
def applyTwice (f : UInt32 → UInt32) (x : UInt32) : UInt32 :=
  f (f x)

def makeAdder (delta : UInt32) : UInt32 → UInt32 :=
  fun x => x + delta

def testClosureApp (inputVal : UInt32) : UInt32 :=
  applyTwice (makeAdder 5) inputVal

@[noinline]
def applyTwiceNoInline (f : UInt32 → UInt32) (x : UInt32) : UInt32 :=
  f (f x)

def testNoInlineClosure (inputVal : UInt32) : UInt32 :=
  applyTwiceNoInline (makeAdder 5) inputVal

def applyBinary (g : UInt32 → UInt32 → UInt32) (a b : UInt32) : UInt32 :=
  g a b

def chooseFn (b : Bool) (f g : UInt32 → UInt32) : UInt32 → UInt32 :=
  if b then f else g

@[kotlin_types "Any?" "UInt" "UInt"]
def applyErased (f : UInt32 → UInt32) (x : UInt32) : UInt32 :=
  f x

