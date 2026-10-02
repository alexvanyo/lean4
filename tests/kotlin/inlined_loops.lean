import Lean.Compiler.Kotlin

/-!
Tests inlined self-tail-recursive loops in the Kotlin backend, including:
- Expansion of @[inline] self-tail-recursive functions into while(true) loops
- Variable mutability (var for variant parameters, invariant parameters aliased)
- Jump-threading and early return forwarding
-/

set_option linter.unusedVariables false

@[inline]
partial def loopSum (n : UInt32) (acc : UInt32) : UInt32 :=
  if n == 0 then
    acc
  else
    loopSum (n - 1) (acc + n)

def computeSum (limit : UInt32) : UInt32 :=
  loopSum limit 0

@[inline]
partial def findIndexLoop (limit : UInt32) (target : UInt32) (idx : UInt32) : Int32 :=
  if idx >= limit then
    -1
  else if idx == target then
    idx.toInt32
  else
    findIndexLoop limit target (idx + 1)

def searchWithEarlyReturn (limit : UInt32) (target : UInt32) : Int32 :=
  findIndexLoop limit target 0
