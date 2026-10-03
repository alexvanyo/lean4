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

def testThunkEval (x : Nat) : Nat :=
  let t := Thunk.mk (fun _ => x * 2 + 1)
  let t2 := t.map (· + 10)
  t2.get + t2.get

def testThunkPure (x : Nat) : Nat :=
  let t := Thunk.pure (x + 5)
  t.get

def testThunkBind (x : Nat) : Nat :=
  let t := Thunk.mk (fun _ => x + 1)
  let t2 := t.bind (fun y => Thunk.mk (fun _ => y * 3))
  t2.get

def testSTRefBasic (initVal : Nat) : Nat :=
  runST fun σ => do
    let r ← ST.mkRef (σ := σ) initVal
    let v0 ← r.get
    r.set (v0 + 10)
    r.modify (· * 2)
    let old ← r.swap 99
    let v1 ← r.get
    return old + v1

def testSTRefModifyGet (n : Nat) : Nat × Nat :=
  runST fun σ => do
    let r ← ST.mkRef (σ := σ) n
    let out ← r.modifyGet fun v => (v + 1, v * 3)
    let final ← r.get
    return (out, final)

def testSTRefPtrEq (x : Nat) : Bool × Bool :=
  runST fun σ => do
    let r1 ← ST.mkRef (σ := σ) x
    let r2 ← ST.mkRef (σ := σ) x
    let eqSelf ← r1.ptrEq r1
    let eqOther ← r1.ptrEq r2
    return (eqSelf, eqOther)

