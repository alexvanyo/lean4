import Lean.Compiler.Kotlin

/-!
Tests join points, variable aliasing, and preservation of non-unit let bindings
in the Kotlin backend.
-/

set_option linter.unusedVariables false

def testJoinPointAliases (flag : Bool) (x y : UInt32) : UInt32 :=
  let z := if flag then x + y else x - y
  let w := z * 2
  w + 1

def testNestedJoinPoints (cond1 cond2 : Bool) (a b c : Int32) : Int32 :=
  let r1 :=
    if cond1 then
      if cond2 then a + b else a - b
    else
      if cond2 then b + c else b - c
  r1 * 2

def testPreserveLet (x : UInt32) : UInt32 :=
  let sideVal := x + 10
  let res := sideVal + 5
  res
