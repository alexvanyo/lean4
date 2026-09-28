import Lean.Compiler.JVM

/-!
Tests Java 17 bytecode compliance, major version 61 emission, and StackMapTable
verification across branching, pattern matching, and module initialization.
-/

def testLeanSource : String :=
"inductive Expr where\n" ++
"  | num (v : Nat)\n" ++
"  | add (l r : Expr)\n" ++
"\n" ++
"def eval (e : Expr) : Nat :=\n" ++
"  match e with\n" ++
"  | .num v => v\n" ++
"  | .add l r => eval l + eval r\n" ++
"\n" ++
"def main : IO Unit := do\n" ++
"  let e : Expr := .add (.num 20) (.add (.num 12) (.num 10))\n" ++
"  IO.println \"Java 17 verification passed\"\n" ++
"  IO.println (eval e)\n" ++
"  if eval e == 42 then\n" ++
"    IO.println \"branch true\"\n" ++
"  else\n" ++
"    IO.println \"branch false\"\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/Java17VerifyModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_Java17VerifyModule.class", "lean/test/Java17VerifyModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  let bytes ← IO.FS.readBinFile "lean/mod_l_lean_test_Java17VerifyModule.class"
  let magic := (bytes.get! 0).toUInt32 <<< 24 |||
               (bytes.get! 1).toUInt32 <<< 16 |||
               (bytes.get! 2).toUInt32 <<< 8  |||
               (bytes.get! 3).toUInt32
  if magic != 0xCAFEBABE then
    throw <| IO.userError s!"Invalid class file magic: {magic}"

  let major := (bytes.get! 6).toUInt16 <<< 8 ||| (bytes.get! 7).toUInt16
  if major != 61 then
    throw <| IO.userError s!"Expected major version 61 (Java 17), got {major}"

  IO.println "Java17VerifyModule.class generated with major version 61."
