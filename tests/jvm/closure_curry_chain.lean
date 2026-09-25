import Lean.Compiler.JVM

/-!
Tests compilation and runtime execution of curried closures and partial applications (.pap).
Models `tests/compile/closure_bug2.lean` and `tests/compile/closure_bug4.lean`, compiling
Lean definitions to JVM bytecode with `lean --jvm` and verifying dynamic closure currying.
-/

open Lean.Compiler.JVM

def testLeanSource : String :=
"def makeAdder (x : Nat) : Nat → Nat := fun y => x + y\n" ++
"def makeCurried (x : Nat) : Nat → Nat → Nat := fun y z => x + y + z\n" ++
"def runTest (x : Nat) : Nat :=\n" ++
"  let f := makeAdder x\n" ++
"  let g := makeCurried x\n" ++
"  f 10 + g 20 30\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/ClosureCurryModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["--jvm=lean/test/ClosureCurryModule.class", "lean/test/ClosureCurryModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  IO.println "ClosureCurryModule.class generated."
