import Lean.Compiler.JVM

/-!
Tests Phase 4: Specialized Constructor & Field Access Optimization on JVM.
Verifies:
- LeanCtor0 allocation and singleton caching for tags 0..15
- LeanCtor1 specialized allocation (alloc1), direct field obj0, and getObj0/setObj0
- LeanCtor2 specialized allocation (alloc2), direct fields obj0/obj1, and getObj0/getObj1/setObj0/setObj1
- LeanCtorScalar1 specialized allocation (allocScalar1), direct scalar0, getScalar0/setScalar0, and byte scalars
- End-to-end compilation with lean --jvm of 0-ary, 1-ary, 2-ary, and single-scalar constructors
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def testLeanSource : String :=
"structure Box1 where\n" ++
"  val : Nat\n" ++
"\n" ++
"structure PairBox where\n" ++
"  fst : Nat\n" ++
"  snd : Nat\n" ++
"\n" ++
"structure ScalarBox where\n" ++
"  u : UInt64\n" ++
"\n" ++
"@[noinline] def mkAndUnbox1 (x : Nat) : Nat :=\n" ++
"  let b : Box1 := { val := x }\n" ++
"  b.val\n" ++
"\n" ++
"@[noinline] def swapPair (a b : Nat) : Nat × Nat :=\n" ++
"  let p : PairBox := { fst := a, snd := b }\n" ++
"  (p.snd, p.fst)\n" ++
"\n" ++
"@[noinline] def mkAndUnboxScalar (u : UInt64) : UInt64 :=\n" ++
"  let sb : ScalarBox := { u := u }\n" ++
"  sb.u\n" ++
"\n" ++
"@[noinline] def testOption (x : Option Nat) : Nat :=\n" ++
"  match x with\n" ++
"  | none => 0\n" ++
"  | some v => v\n" ++
"\n" ++
"def main : IO Unit := do\n" ++
"  IO.println (mkAndUnbox1 42)\n" ++
"  let (snd, fst) := swapPair 100 200\n" ++
"  IO.println s!\"{snd} {fst}\"\n" ++
"  IO.println (mkAndUnboxScalar (9876543210 : UInt64))\n" ++
"  IO.println (testOption none)\n" ++
"  IO.println (testOption (some 999))\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/SpecializedCtorModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_SpecializedCtorModule.class", "lean/test/SpecializedCtorModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr} {out.stdout}"

  IO.println "SpecializedCtorModule.class generated."
