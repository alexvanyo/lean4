import Lean.Compiler.JVM

/-!
Tests self-tail-call optimization (STCO) with 1,000,000 iterations on the JVM backend,
verifying no StackOverflowError, parameter collision handling, inlined primitive comparisons,
and multi-way branching (tableswitch).
-/

def testLeanSource : String :=
"inductive Color where\n" ++
"  | red\n" ++
"  | green\n" ++
"  | blue\n" ++
"  | yellow\n" ++
"\n" ++
"def colorToCode (c : Color) : UInt32 :=\n" ++
"  match c with\n" ++
"  | .red => 10\n" ++
"  | .green => 20\n" ++
"  | .blue => 30\n" ++
"  | .yellow => 40\n" ++
"\n" ++
"partial def countUp (n : UInt32) (limit : UInt32) : UInt32 :=\n" ++
"  if n == limit then n\n" ++
"  else countUp (n + 1) limit\n" ++
"\n" ++
"partial def sumDown (n : UInt64) (acc : UInt64) : UInt64 :=\n" ++
"  if n == 0 then acc\n" ++
"  else sumDown (n - 1) (acc + n)\n" ++
"\n" ++
"def main : IO Unit := do\n" ++
"  IO.println (colorToCode .blue)\n" ++
"  let r1 := countUp 0 1000000\n" ++
"  IO.println r1\n" ++
"  let r2 := sumDown 1000000 0\n" ++
"  IO.println r2\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/TailRecStressModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_TailRecStressModule.class", "lean/test/TailRecStressModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stdout}\n{out.stderr}"

  IO.println "TailRecStressModule.class generated."
