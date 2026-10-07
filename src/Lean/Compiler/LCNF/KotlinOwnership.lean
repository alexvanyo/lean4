/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Lean FRO, LLC
-/
module

prelude
public import Lean.Compiler.LCNF.CompilerM
public import Lean.Compiler.LCNF.PhaseExt
import Lean.Compiler.KotlinAttrs
import Lean.Compiler.ExportAttr
import Lean.Compiler.ExternAttr

/-!
# Exclusive-ownership check for the Kotlin backend

Lean values are immutable. The C runtime nevertheless updates arrays and constructor cells in place
when they are exclusively owned (reference count 1), which it checks at run time (`isShared`,
`Array.set!`, ...). The Kotlin backend has no reference counts: it *always* updates in place. That is
only faithful to the Lean semantics if every such update happens on an exclusively owned value.

This module verifies that statically by abstract interpretation of the reference-counting
instructions (`inc`/`dec`) of the impure code. Every value is mapped to an abstract object with a
known reference count (or unknown). Updates in place (`isShared`, `reset`, `Array.set!`,
`Array.uset`) *claim* that the object has reference count 1; a claim that cannot be verified is an
error, which fails the build.

Exclusive ownership of parameters is inferred: a parameter whose object is claimed is marked as
*exclusive*, which turns into a claim at each call site. Results that are identical to a parameter
(the parameter itself, or a component of a `Prod.mk` result) are recorded as well, so that callers
can keep tracking the object; the Kotlin backend uses this to drop such results. Summaries are
computed as a fixpoint over all declarations.
-/

public section

namespace Lean.Compiler.LCNF.Kotlin.Ownership

/-- Reference count of an abstract object: exact, or unknown. -/
inductive RC where
  | exact (n : Nat)
  | top
  deriving Inhabited, BEq

def RC.add : RC → Nat → RC
  | .exact k, n => .exact (k + n)
  | .top, _ => .top

structure Obj where
  rc : RC
  /-- Known objects in object fields: `(field index, object id)`. -/
  children : Array (Nat × Nat) := #[]
  /-- Object fields that are not in `children` hold exclusively owned objects (recursively). -/
  deep : Bool := false
  /--
  Object of parameter `j` (or reachable from it through fields) whose parameter has not been
  assumed exclusive yet and whose count has not been touched: a failing claim on it marks `j` as
  exclusive instead of failing.
  -/
  pristine? : Option Nat := none
  /-- Constructor that allocated the object. -/
  ctor? : Option Name := none
  /-- Result of a call whose result shape is not known yet (see `RetShape.none`). -/
  pending : Bool := false
  /-- Object whose field this object was read from. -/
  parent? : Option Nat := none
  /-- Whether this object is an instance of a `@[mutable_kotlin_class]` structure. -/
  isMutableClass : Bool := false
  /-- Whether this object is a deeply immutable Kotlin value (e.g. `String`, `Nat`, scalar, function). -/
  immutable : Bool := false
  /-- Whether the initial persistent reference of a 0-arg closed constant has been consumed. -/
  persistentUsed : Bool := false
  deriving Inhabited

/-- Shape of the results of a declaration, joined over its `return`s. -/
inductive RetShape where
  /-- No `return` analyzed yet. -/
  | none
  /-- The result is a freshly allocated, exclusively owned value. -/
  | fresh
  /-- The result is (the object of) parameter `j`. -/
  | param (j : Nat)
  /-- The result is a `Prod.mk` whose components are parameters (`some j`) or unknown. -/
  | prod (comps : Array (Option Nat))
  | unknown
  deriving Inhabited, BEq, Repr

def RetShape.isFreshOrExclusiveParam (exclusive : Array Bool) : RetShape → Bool
  | .fresh => true
  | .param i => exclusive[i]?.getD false
  | _ => false

def RetShape.join (exclusive : Array Bool) (s1 s2 : RetShape) : RetShape :=
  match s1, s2 with
  | .none, s | s, .none => s
  | .fresh, .fresh => .fresh
  | .param i, .param j =>
    if i == j then .param i
    else if (exclusive[i]?.getD false) && (exclusive[j]?.getD false) then .fresh
    else .unknown
  | .prod a, .prod b =>
    if a.size == b.size then .prod (a.zipWith (fun x y => if x == y then x else Option.none) b)
    else .unknown
  | a, b =>
    if a.isFreshOrExclusiveParam exclusive && b.isFreshOrExclusiveParam exclusive then .fresh
    else .unknown

/-- Parameter `j` identical to the `i`-th component (`none`: the whole result) of the result. -/
def RetShape.identities : RetShape → Array (Option Nat × Nat)
  | .param j => #[(Option.none, j)]
  | .prod comps => Id.run do
    let mut r := #[]
    for i in [:comps.size] do
      if let some j := comps[i]! then r := r.push (some i, j)
    return r
  | _ => #[]

structure Summary where
  /-- Parameters whose argument must be exclusively owned (recursively through fields). -/
  exclusive : Array Bool
  ret : RetShape := .none
  deriving Inhabited, BEq

structure PathState where
  objs : Array Obj := #[]
  vars : Std.HashMap FVarId Nat := {}
  /-- Variables known to be `false` (results of `isShared` on exclusively owned mutable objects). -/
  knownFalse : Std.HashSet FVarId := {}
  /-- Variables known to be `true` (results of `isShared` on immutable objects). -/
  knownTrue : Std.HashSet FVarId := {}
  deriving Inhabited

structure Ctx where
  decl : Decl .impure
  summaries : Std.HashMap Name Summary
  localParams : Std.HashMap Name (Array (Param .impure))
  mutableClasses : Std.HashSet String := {}
  jps : Std.HashMap FVarId (FunDecl .impure) := {}
  /-- Object ids of the parameters at entry. -/
  paramObjs : Array Nat := #[]

def isMutableClassType (mutableClasses : Std.HashSet String) (ty : Expr) : Bool :=
  match ImpureType.getJvmTypeDesc? ty with
  | some d => d.startsWith "kotlin:" && mutableClasses.contains (d.drop 7).toString
  | none => false

structure Out where
  requireExclusive : Std.HashSet Nat := {}
  errors : Array MessageData := #[]
  ret : RetShape := .none

abbrev M := ReaderT Ctx (StateRefT Out CompilerM)

def newObj (st : PathState) (o : Obj) : PathState × Nat :=
  ({ st with objs := st.objs.push o }, st.objs.size)

def objOf (st : PathState) (x : FVarId) : PathState × Nat :=
  match st.vars[x]? with
  | some i => (st, i)
  | none =>
    let (st, i) := newObj st { rc := .top }
    ({ st with vars := st.vars.insert x i }, i)

def bind (st : PathState) (x : FVarId) (i : Nat) : PathState :=
  { st with vars := st.vars.insert x i }

def modifyObj (st : PathState) (i : Nat) (f : Obj → Obj) : PathState :=
  { st with objs := st.objs.modify i f }

def error (msg : MessageData) : M Unit := do
  let declName := (← read).decl.name
  modify fun o => { o with errors := o.errors.push m!"Kotlin backend: in `{declName}`: {msg}" }

partial def isDeeplyImmutableMonoType (t : Expr) : Bool :=
  match t with
  | .const n _ =>
    n == ``Nat || n == ``Int || n == ``String || n == ``Bool || n == ``Decidable ||
    n == ``Char || n == ``Unit || n == ``PUnit ||
    n == ``UInt8 || n == ``UInt16 || n == ``UInt32 || n == ``UInt64 || n == ``USize ||
    n == ``Int8 || n == ``Int16 || n == ``Int32 || n == ``Int64 || n == ``ISize ||
    n == ``Float || n == ``Float32 || n == ``lcErased || n == ``lcVoid
  | .forallE .. => true
  | .app (.app (.const ``Prod _) a) b =>
    isDeeplyImmutableMonoType a && isDeeplyImmutableMonoType b
  | _ => false

/--
Checks that object `i` is exclusively owned (and, if `deep`, everything reachable through its
fields). Returns `false` after reporting an error or requesting a parameter to be exclusive.
-/
partial def claim (st : PathState) (i : Nat) (deep : Bool) (what : MessageData) : M Bool := do
  let o := st.objs[i]!
  if o.immutable && (deep || o.rc == .exact 1) then return true
  if let some j := o.pristine? then
    modify fun out => { out with requireExclusive := out.requireExclusive.insert j }
    return false
  unless o.rc == .exact 1 do
    error m!"{what} requires exclusive ownership, but the value may be shared"
    return false
  if deep then
    unless o.deep || o.ctor?.isSome do
      error m!"{what} requires exclusive ownership of all fields, but they may be shared"
      return false
    for (_, c) in o.children do
      unless ← claim st c true what do return false
  return true

/-- `dec` of object `i`: releasing the last reference releases its known fields. -/
partial def decObj (st : PathState) (i : Nat) : PathState :=
  let o := st.objs[i]!
  match o.rc with
  | .exact (k + 1) =>
    let st := modifyObj st i fun o => { o with rc := .exact k }
    if k == 0 then o.children.foldl (fun st (_, c) => decObj st c) st else st
  | _ => st

/-- Makes the count of object `i` unknown (a reference escaped to code we do not track). -/
def escape (st : PathState) (i : Nat) : PathState :=
  modifyObj st i fun o => { o with rc := .top, pristine? := none }

def isArraySet (fn : Name) : Bool :=
  let fn := match fn with | .str p "_boxed" => p | _ => fn
  fn == ``Array.set || fn == ``Array.set! || fn == ``Array.uset || fn == `Array.setIfInBounds || fn == `Array.fset ||
    fn == ``Array.swap || fn == ``Array.swapIfInBounds || fn == `Array.uswap ||
    (fn.isStr && fn.getString! == "aset")

def isScalarArraySet (fn : Name) : Bool :=
  let fn := match fn with | .str p "_boxed" => p | _ => fn
  fn == `ByteArray.set! || fn == `ByteArray.set || fn == `ByteArray.uset ||
    fn == `FloatArray.set! || fn == `FloatArray.set || fn == `FloatArray.uset

def isArrayAlloc (fn : Name) : Bool :=
  let fn := match fn with | .str p "_boxed" => p | _ => fn
  fn == ``Array.replicate || fn == `Array.mkArray || fn == ``Array.mkEmpty ||
    fn == ``Array.emptyWithCapacity || fn == ``Array.empty ||
    fn == ``Array.mk || fn == ``List.toArray || fn == `List.toArrayImpl || fn == `List.toArrayImpl._redArg ||
    fn == ``Array.push || fn == ``Array.pop ||
    fn == ``Array.append || fn == `Array.append._redArg ||
    fn == ``Array.appendCore || fn == `Array.appendCore._redArg ||
    fn == ``Array.extract || fn == `Array.extract._redArg ||
    fn == `Array.markLinear || fn == `Array.markLinear._redArg ||
    fn == ``Array.mkArray0 || fn == ``Array.mkArray1 || fn == ``Array.mkArray2 ||
    fn == ``Array.mkArray3 || fn == ``Array.mkArray4 || fn == ``Array.mkArray5 ||
    fn == ``Array.mkArray6 || fn == ``Array.mkArray7 || fn == ``Array.mkArray8 ||
    fn == `ByteArray.emptyWithCapacity || fn == `ByteArray.empty ||
    fn == `ByteArray.mk || fn == `ByteArray.data || fn == `ByteArray.push ||
    fn == `ByteArray.copySlice || fn == `ByteArray.extract ||
    fn == `ByteArray.fastAppend || fn == `ByteArray.append ||
    fn == `ByteArray.markLinear || fn == `String.toUTF8 || fn == `String.toByteArray ||
    fn == `FloatArray.emptyWithCapacity || fn == `FloatArray.empty ||
    fn == `FloatArray.mk || fn == `FloatArray.data || fn == `FloatArray.push ||
    fn == `FloatArray.markLinear ||
    (fn.isStr && (fn.getString!.startsWith "alloc" || fn.getString!.startsWith "empty"))

def isPropagateMark (fn : Name) : Bool :=
  let fn := match fn with | .str p "_boxed" => p | _ => fn
  fn == ``Array.propagateMark || fn == `Array.propagateMark._redArg ||
    fn == `ByteArray.propagateMark || fn == `FloatArray.propagateMark ||
    fn == `String.propagateMark

def isImmutablePrimOp (fn : Name) : Bool :=
  let fn := match fn with | .str p "_boxed" => p | _ => fn
  let fn := match fn with | .str p "_redArg" => p | _ => fn
  let p := fn.getPrefix
  p == ``Nat || p == ``Int || p == ``String || p == ``String.Slice ||
    p == ``Char || p == ``Bool ||
    p == ``UInt8 || p == ``UInt16 || p == ``UInt32 || p == ``UInt64 || p == ``USize ||
    p == ``Int8 || p == ``Int16 || p == ``Int32 || p == ``Int64 || p == ``ISize ||
    p == `Float || p == `Float32 ||
    fn == ``mixHash

/--
`@[extern "kotlin_inplace:<template>"]`: a Kotlin statement that updates its first argument, which
must be exclusively owned, in place; the result is that argument.
-/
def isInplaceExtern (env : Environment) (fn : Name) : Bool :=
  match getExternNameFor env `kotlin fn with
  | some s => s.startsWith "kotlin_inplace:"
  | none => false

/-- Index of the argument updated in place by `fn`, if `fn` is an in-place update. -/
def inplaceArg? (env : Environment) (fn : Name) (args : Array (Arg .impure)) : Option Nat :=
  if isArraySet fn then some 1
  else if isScalarArraySet fn then some 0
  else if isInplaceExtern env fn then args.findIdx? (· matches .fvar _)
  else none

def paramsOf? (fn : Name) : M (Option (Array (Param .impure))) := do
  if let some ps := (← read).localParams[fn]? then return some ps
  return (← getImpureSignature? fn).map (·.params)

/-- Owned arguments move into the callee; borrowed ones are unaffected. -/
def consumeArgs (st : PathState) (fn? : Option Name) (args : Array (Arg .impure)) : M PathState := do
  let ps? ← match fn? with
    | some fn => paramsOf? fn
    | none => pure none
  let mut st := st
  for h : k in [:args.size] do
    if let .fvar a := args[k] then
      let borrowed : Bool := match ps? with
        | some ps => (ps[k]?.map (·.borrow)).getD false
        | none => false
      if !borrowed then
        let (st', i) := objOf st a
        st := escape st' i
  return st

/--
Kotlin object fields of the class-struct constructor application `args`, as `(field index, object)`.
-/
def ctorChildren (st : PathState) (args : Array (Arg .impure)) : PathState × Array (Nat × Nat) := Id.run do
  let mut children := #[]
  let mut st := st
  for h : k in [:args.size] do
    if let .fvar a := args[k] then
      let (st', c) := objOf st a
      st := st'
      children := children.push (k, c)
  return (st, children)

/--
The value updated in place by a constructor application `args` of a `@[kotlin_class]` structure:
the unique value that the object fields in `args` were read from (e.g. `{ s with f := v }` reads the
other fields of `s`). The Kotlin backend has no allocation for such structures; see
`updatedByClassCtor?`.
-/
def classCtorSource? (st : PathState) (args : Array (Arg .impure)) : Option Nat := Id.run do
  let mut src? : Option Nat := none
  for a in args do
    if let .fvar a := a then
      if let some i := st.vars[a]? then
        if let some p := st.objs[i]!.parent? then
          match src? with
          | none => src? := some p
          | some q => if q != p then return none
  if src?.isNone then
    for o in st.objs do
      if let some p := o.parent? then
        if st.objs[p]!.isMutableClass then
          match src? with
          | none => src? := some p
          | some q => if q != p then return none
  return src?

/--
A constructor application of a `@[mutable_kotlin_class]` structure updates the value it reads its other
fields from in place. That is only faithful if that value is no longer referenced: its count must
have dropped to 0.
-/
def visitClassCtor? (st : PathState) (x : FVarId) (info : CtorInfo) (args : Array (Arg .impure))
    (scalars : Array FVarId) : M (Option PathState) := do
  let structName := info.name.getPrefix
  unless isMutableKotlinClass (← getEnv) structName do return none
  let some p := classCtorSource? st (args ++ scalars.map .fvar)
    | error m!"constructing a `{structName}` requires an update of an existing value (e.g. \
        `\{ s with f := v }` with some field of `s` unchanged)"
      return some st
  let o := st.objs[p]!
  if let some j := o.pristine? then
    modify fun out => { out with requireExclusive := out.requireExclusive.insert j }
  else unless o.rc == .exact 0 do
    error m!"updating a `{structName}` in place, but the updated value may still be referenced"
  let (st, children) := ctorChildren st args
  let st := modifyObj st p fun o =>
    { o with rc := .exact 1, children, deep := true, ctor? := some info.name, pristine? := none, isMutableClass := true }
  return some (bind st x p)

/-- Values stored into scalar fields of `x` right after its construction (continuation `k`). -/
partial def ctorScalars (x : FVarId) (k : Code .impure) : Array FVarId :=
  match k with
  | .sset y _ _ v _ k _ | .uset y _ v k _ =>
    if y == x then #[v] ++ ctorScalars x k else ctorScalars x k
  | .inc (k := k) .. | .dec (k := k) .. => ctorScalars x k
  | _ => #[]

mutual

partial def visitCode (st : PathState) (code : Code .impure) : M Unit := do
  match code with
  | .let decl k => visitCode (← visitLet st decl k) k
  | .inc x n _ persistent k =>
    let (st, i) := objOf st x
    -- A pristine object stays pristine: its count is unknown until its parameter is exclusive.
    -- For a 0-arg closed constant (`ExplicitRC` marks it borrowed and emits `inc[persistent]`
    -- before each owned consumption without a matching `dec`), the first `inc[persistent]` with
    -- `n == 1` transfers ownership of the freshly created constant without bumping `rc` to 2.
    let st := modifyObj st i fun o =>
      if persistent && !o.persistentUsed && n == 1 then
        { o with persistentUsed := true }
      else
        { o with rc := o.rc.add n, persistentUsed := o.persistentUsed || persistent }
    visitCode st k
  | .dec x _ _ _ _ k =>
    let (st, i) := objOf st x
    visitCode (decObj st i) k
  | .oset x idx y k =>
    let (st, i) := objOf st x
    let st ← match y with
      | .fvar y =>
        let (st, c) := objOf st y
        pure <| modifyObj st i fun o =>
          { o with children := (o.children.filter (·.1 != idx)).push (idx, c) }
      | .erased => pure st
    visitCode st k
  | .sset (k := k) .. | .uset (k := k) .. | .setTag (k := k) .. | .del (k := k) .. =>
    visitCode st k
  | .jp decl k =>
    withReader (fun ctx => { ctx with jps := ctx.jps.insert decl.fvarId decl }) (visitCode st k)
  | .jmp j args =>
    let some decl := (← read).jps[j]? | return
    let mut st := st
    for p in decl.params, a in args do
      match a with
      | .fvar a =>
        let (st', i) := objOf st a
        st := bind st' p.fvarId i
        if st.knownFalse.contains a then st := { st with knownFalse := st.knownFalse.insert p.fvarId }
        if st.knownTrue.contains a then st := { st with knownTrue := st.knownTrue.insert p.fvarId }
      | .erased => pure ()
    visitCode st decl.value
  | .cases cs =>
    if st.knownFalse.contains cs.discr then
      let has0 := cs.alts.any fun | .ctorAlt info _ => info.cidx == 0 | .default _ => false
      for alt in cs.alts do
        match alt with
        | .ctorAlt info c => if info.cidx == 0 then visitCode st c
        | .default c => unless has0 do visitCode st c
    else if st.knownTrue.contains cs.discr then
      let has1 := cs.alts.any fun | .ctorAlt info _ => info.cidx == 1 | .default _ => false
      for alt in cs.alts do
        match alt with
        | .ctorAlt info c => if info.cidx == 1 then visitCode st c
        | .default c => unless has1 do visitCode st c
    else
      for alt in cs.alts do visitCode st alt.getCode
  | .return x => visitReturn st x
  | .unreach _ => pure ()

partial def visitLet (st : PathState) (decl : LetDecl .impure) (k : Code .impure) : M PathState := do
  let x := decl.fvarId
  let isMut := isMutableClassType (← read).mutableClasses decl.type
  match decl.value with
  | .oproj idx y =>
    let (st, i) := objOf st y
    let o := st.objs[i]!
    if let some (_, c) := o.children.find? (·.1 == idx) then
      return bind st x c
    let child : Obj :=
      if o.deep && o.rc == .exact 1 then { rc := .exact 1, deep := true, parent? := some i, isMutableClass := isMut }
      else if let some j := o.pristine? then { rc := .top, pristine? := some j, parent? := some i, isMutableClass := isMut }
      else { rc := .top, parent? := some i, isMutableClass := isMut }
    let (st, c) := newObj st child
    let st := modifyObj st i fun o => { o with children := o.children.push (idx, c) }
    return bind st x c
  | .ctor info args =>
    if let some st' ← visitClassCtor? st x info args (ctorScalars x k) then return st'
    let mut children := #[]
    let mut st1 := st
    for h : k in [:args.size] do
      if let .fvar a := args[k] then
        let (st', c) := objOf st1 a
        st1 := st'
        children := children.push (k, c)
    let (st2, i) := newObj st1 { rc := .exact 1, children, deep := true, ctor? := some info.name, isMutableClass := isMut }
    return bind st2 x i
  | .reset _ y =>
    let (st, i) := objOf st y
    if ← claim st i false m!"`reset` (in-place constructor update)" then
      let o := st.objs[i]!
      let st := o.children.foldl (fun st (_, c) => decObj st c) st
      let st := modifyObj st i fun o => { o with children := #[], deep := true }
      return bind st x i
    return st
  | .reuse y info _ args =>
    let (st0, i) := objOf st y
    let mut children := #[]
    let mut st1 := st0
    for h : k in [:args.size] do
      if let .fvar a := args[k] then
        let (st', c) := objOf st1 a
        st1 := st'
        children := children.push (k, c)
    let st2 := modifyObj st1 i fun o =>
      { o with rc := .exact 1, children, deep := true, ctor? := some info.name }
    return bind st2 x i
  | .isShared y =>
    let (st, i) := objOf st y
    if st.objs[i]!.isMutableClass then
      if ← claim st i false m!"in-place constructor update" then
        return { st with knownFalse := st.knownFalse.insert x }
      return st
    else
      return { st with knownTrue := st.knownTrue.insert x }
  | .fap fn args =>
    if isPropagateMark fn then
      if let some (.fvar a) := args.back? then
        let (st, i) := objOf st a
        return bind st x i
    if let some ai := inplaceArg? (← getEnv) fn args then
      if let some (Arg.fvar a) := args[ai]? then
        let (st, i) := objOf st a
        discard <| claim st i false m!"`{fn}`"
        let st ← consumeArgs st none ((args.extract 0 ai) ++ (args.extract (ai + 1) args.size))
        return bind st x i
    if args.isEmpty && (getExternNameFor (← getEnv) `kotlin fn).any (·.startsWith "kotlin_expr:") then
      let (st, i) := newObj st { rc := .top, isMutableClass := isMut, immutable := true }
      return bind st x i
    if isArrayAlloc fn then
      let st ← consumeArgs st none args
      let (st, i) := newObj st { rc := .exact 1, deep := true, isMutableClass := isMut }
      return bind st x i
    if let some summary := (← read).summaries[fn]? then
      return ← visitCall st x fn args summary isMut
    let st ← consumeArgs st (some fn) args
    if isImmutablePrimOp fn then
      let (st, i) := newObj st { rc := .exact 1, deep := true, isMutableClass := isMut, immutable := true }
      return bind st x i
    let (st, i) := newObj st { rc := .top, isMutableClass := isMut }
    return bind st x i
  | .pap _ args | .fvar _ args =>
    let st ← consumeArgs st none args
    let (st, i) := newObj st { rc := .top, isMutableClass := isMut }
    return bind st x i
  | .sproj _ _ y | .uproj _ y =>
    let (st, p) := objOf st y
    let (st, i) := newObj st { rc := .top, parent? := some p, isMutableClass := isMut, immutable := true }
    return bind st x i
  | .lit _ | .box _ _ =>
    let (st, i) := newObj st { rc := .exact 1, deep := true, isMutableClass := isMut, immutable := true }
    return bind st x i
  | _ =>
    let (st, i) := newObj st { rc := .top, isMutableClass := isMut }
    return bind st x i

partial def visitCall (st : PathState) (x : FVarId) (fn : Name) (args : Array (Arg .impure))
    (summary : Summary) (isMut : Bool) : M PathState := do
  let ps := (← read).localParams[fn]?.getD #[]
  let mut st := st
  let mut argObjs : Array (Option Nat) := #[]
  for h : k in [:args.size] do
    match args[k] with
    | .fvar a =>
      let (st', i) := objOf st a
      st := st'
      argObjs := argObjs.push (some i)
      if summary.exclusive[k]?.getD false then
        discard <| claim st i true m!"argument {k + 1} of `{fn}`"
    | .erased => argObjs := argObjs.push none
  -- Arguments that come back as part of the result keep their count if the callee requires them
  -- exclusive (it returns them exclusive); all other owned arguments escape.
  let ids := summary.ret.identities
  for h : k in [:args.size] do
    if let some i := argObjs[k]! then
      let borrowed : Bool := (ps[k]?.map (·.borrow)).getD false
      let returned := ids.any (·.2 == k)
      let exclusive := summary.exclusive[k]?.getD false
      if returned && exclusive then
        -- The callee returns it exclusively owned (recursively), possibly with replaced fields.
        st := modifyObj st i fun o =>
          { o with rc := .exact 1, children := #[], deep := true, pristine? := none, ctor? := none }
      else if !borrowed then
        st := escape st i
  match summary.ret with
  | .none =>
    let (st2, i) := newObj st { rc := .top, pending := true, isMutableClass := isMut }
    return bind st2 x i
  | .fresh =>
    let (st2, i) := newObj st { rc := .exact 1, deep := true, isMutableClass := isMut }
    return bind st2 x i
  | .param j =>
    match argObjs[j]? with
    | some (some i) => return bind st x i
    | _ =>
      let (st2, i) := newObj st { rc := .top, isMutableClass := isMut }
      return bind st2 x i
  | .prod comps =>
    let mut children := #[]
    for h : c in [:comps.size] do
      match comps[c] with
      | some j =>
        if let some (some i) := argObjs[j]? then children := children.push (c, i)
      | none =>
        let (st', i) := newObj st { rc := .top }
        st := st'
        children := children.push (c, i)
    let (st2, i) := newObj st { rc := .exact 1, children, deep := true, ctor? := some ``Prod.mk }
    return bind st2 x i
  | .unknown =>
    let (st2, i) := newObj st { rc := .top, isMutableClass := isMut }
    return bind st2 x i

partial def visitReturn (st : PathState) (x : FVarId) : M Unit := do
  let (st, i) := objOf st x
  let o := st.objs[i]!
  if o.pending then return
  let ctx ← read
  let paramOf? (i : Nat) : Option Nat := ctx.paramObjs.findIdx? (· == i)
  let shape : RetShape :=
    if let some j := paramOf? i then .param j
    else if o.ctor? == some ``Prod.mk then
      .prod ((List.range 2).toArray.map fun c =>
        (o.children.find? (·.1 == c)).bind fun (_, ci) => paramOf? ci)
    else if o.rc == .exact 1 && (o.deep || o.ctor?.isSome) &&
            o.children.all (fun (_, c) => st.objs[c]!.immutable || (st.objs[c]!.rc == .exact 1 && (st.objs[c]!.deep || st.objs[c]!.ctor?.isSome))) then
      .fresh
    else .unknown
  -- Parameters returned as (part of) the result must come back exclusive if they came in exclusive.
  let summary := ctx.summaries[ctx.decl.name]?.getD { exclusive := #[] }
  for (c?, j) in shape.identities do
    if summary.exclusive[j]?.getD false then
      let obj := match c? with
        | none => i
        | some c => ((o.children.find? (·.1 == c)).map (·.2)).getD i
      discard <| claim st obj true m!"returning parameter {j + 1}"
  modify fun out => { out with ret := out.ret.join summary.exclusive shape }

end

def analyzeDecl (decl : Decl .impure) (summaries : Std.HashMap Name Summary)
    (localParams : Std.HashMap Name (Array (Param .impure)))
    (mutableClasses : Std.HashSet String) : CompilerM Out := do
  let .code code := decl.value | return {}
  let monoDecl? ← getMonoDecl? decl.name
  let summary := summaries[decl.name]?.getD { exclusive := decl.params.map fun _ => false }
  let mut st : PathState := {}
  let mut paramObjs := #[]
  for h : j in [:decl.params.size] do
    let p := decl.params[j]
    let isMut := isMutableClassType mutableClasses p.type
    let isImm := match monoDecl?.bind (·.params[j]?) with
      | some mp => isDeeplyImmutableMonoType mp.type
      | none => false
    let o : Obj :=
      if isImm then { rc := .exact 1, deep := true, isMutableClass := false, immutable := true }
      else if p.borrow then { rc := .top, isMutableClass := isMut }
      else if summary.exclusive[j]?.getD false then { rc := .exact 1, deep := true, isMutableClass := isMut }
      else { rc := .top, pristine? := some j, isMutableClass := isMut }
    let (st', i) := newObj st o
    st := bind st' p.fvarId i
    paramObjs := paramObjs.push i
  let ctx : Ctx := { decl, summaries, localParams, mutableClasses, paramObjs }
  let ((), out) ← (visitCode st code).run ctx |>.run {}
  return out

/-- Result of the ownership analysis. -/
structure Result where
  summaries : Std.HashMap Name Summary
  errors : Array MessageData

/--
Computes ownership summaries for `decls` and checks all in-place updates. `external` returns
`true` for declarations called from outside the analyzed code (Kotlin members, exports, closures),
whose parameters cannot be required to be exclusive except for a Kotlin member receiver.
-/
def analyze (decls : Array (Decl .impure)) (isMember : Name → Bool) (isExternal : Name → Bool) :
    CompilerM Result := do
  let env ← getEnv
  let mutableClasses := env.constants.map₂.foldl (init := ({} : Std.HashSet String)) fun s n _ =>
    match getMutableKotlinClass? env n with
    | some t => s.insert t
    | none => s
  let localParams := decls.foldl (init := ({} : Std.HashMap Name _)) fun m d => m.insert d.name d.params
  let mut summaries : Std.HashMap Name Summary := decls.foldl (init := {}) fun m d =>
    m.insert d.name { exclusive := d.params.map fun _ => false }
  let mut iter := 0
  repeat
    iter := iter + 1
    let mut changed := false
    let mut errors := #[]
    for d in decls do
      let out ← analyzeDecl d summaries localParams mutableClasses
      let old := summaries[d.name]!
      let mut exclusive := old.exclusive
      for j in out.requireExclusive do
        if j < exclusive.size && d.params[j]!.borrow == false then
          exclusive := exclusive.set! j true
        else
          errors := errors.push m!"Kotlin backend: in `{d.name}`: an in-place update requires \
            exclusive ownership of a borrowed parameter"
      let new : Summary := { exclusive, ret := out.ret }
      if new != old then
        summaries := summaries.insert d.name new
        changed := true
      errors := errors ++ out.errors
    if !changed || iter > 100 then
      -- Parameters of declarations called from Kotlin cannot be required to be exclusive, except
      -- for the receiver of a member, which must then be returned so that callers observe the
      -- update.
      for d in decls do
        let s := summaries[d.name]!
        for h : j in [:s.exclusive.size] do
          if s.exclusive[j] then
            let receiver := j == 0 && isMember d.name
            if receiver then
              unless s.ret.identities.any (·.2 == 0) do
                errors := errors.push m!"Kotlin backend: `{d.name}` updates its receiver in \
                  place, so it must return the receiver (e.g. as the last component of its result)"
            else if isExternal d.name then
              errors := errors.push m!"Kotlin backend: `{d.name}` is called from Kotlin, but \
                parameter {j + 1} would need to be exclusively owned"
      return { summaries, errors }
  return { summaries, errors := #[] }

end Lean.Compiler.LCNF.Kotlin.Ownership
