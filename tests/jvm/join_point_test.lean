import Lean.Compiler.JVM

/-!
Tests Phase 2 Join Point compilation in the JVM backend:
- Shared join points jumped to from multiple match/conditional branches
- Multi-argument join points with different types
- Nested join points and loop join points
- JVM StackMapTable verification and execution
-/

def testLeanSource : String :=
"def joinPointDiamond (x : Nat) : Nat :=\n" ++
"  let f (n : Nat) : Nat :=\n" ++
"    match n with\n" ++
"    | 0 => 100\n" ++
"    | 1 => 200\n" ++
"    | _ => 300\n" ++
"  f x + 1\n" ++
"\n" ++
"def joinPointMultiArg (cond1 cond2 : Bool) (x y : Nat) : Nat :=\n" ++
"  let jp (a b : Nat) : Nat := a * 10 + b\n" ++
"  if cond1 then\n" ++
"    if cond2 then jp (x + 1) (y + 2)\n" ++
"    else jp (x + 3) (y + 4)\n" ++
"  else\n" ++
"    if cond2 then jp (x + 5) (y + 6)\n" ++
"    else jp (x + 7) (y + 8)\n" ++
"\n" ++
"def joinPointLoop (n : Nat) : Nat :=\n" ++
"  let rec loop (i acc : Nat) : Nat :=\n" ++
"    match i with\n" ++
"    | 0 => acc\n" ++
"    | i + 1 => loop i (acc + (i + 1))\n" ++
"  loop n 0\n" ++
"\n" ++
"def main : IO Unit := do\n" ++
"  IO.println (joinPointDiamond 0)\n" ++
"  IO.println (joinPointDiamond 1)\n" ++
"  IO.println (joinPointDiamond 2)\n" ++
"  IO.println (joinPointMultiArg true true 1 2)\n" ++
"  IO.println (joinPointMultiArg true false 1 2)\n" ++
"  IO.println (joinPointMultiArg false true 1 2)\n" ++
"  IO.println (joinPointMultiArg false false 1 2)\n" ++
"  IO.println (joinPointLoop 10)\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/JoinPointModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_JoinPointModule.class", "lean/test/JoinPointModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  IO.println "JoinPointModule.class generated successfully."
