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

@[inline]
partial def scanRowAll (pred : UInt32 → Bool) (base : UInt32) (cols : UInt32) (j : UInt32) : Bool :=
  if j < cols then
    if pred (base + j) then
      scanRowAll pred base cols (j + 1)
    else
      false
  else
    true

@[inline]
partial def scanGridAll (pred : UInt32 → Bool) (rows : UInt32) (cols : UInt32) (i : UInt32) : Bool :=
  if i >= rows then
    true
  else if scanRowAll pred (i * cols) cols 0 then
    scanGridAll pred rows cols (i + 1)
  else
    false

def allBelowLimit (rows : UInt32) (cols : UInt32) (limit : UInt32) : Bool :=
  scanGridAll (fun x => x < limit) rows cols 0

def anyEquals (rows : UInt32) (cols : UInt32) (target : UInt32) : Bool :=
  !scanGridAll (fun x => x != target) rows cols 0

