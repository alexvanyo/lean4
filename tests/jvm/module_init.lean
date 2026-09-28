import Lean.Compiler.JVM

/-!
Tests module initialization, top-level `initialize` blocks, and `IO.Ref` / `ST.Ref` primitives on JVM.
Models `tests/compile/init.lean`.
-/

def testLeanSource : String :=
"namespace Foo\n" ++
"initialize ref : IO.Ref Nat ← IO.mkRef 10\n" ++
"initialize vals : IO.Ref (Array String) ← IO.mkRef #[]\n" ++
"def registerVal (s : String) : IO Unit := do\n" ++
"  if (← vals.get).contains s then\n" ++
"    throw $ IO.userError \"value already registered\"\n" ++
"  vals.modify (·.push s)\n" ++
"initialize\n" ++
"  IO.println \"started the program\"\n" ++
"  ref.modify (· + 20)\n" ++
"  registerVal \"hello\"\n" ++
"initialize\n" ++
"  registerVal \"world\"\n" ++
"initialize\n" ++
"  registerVal \"foo\"\n" ++
"end Foo\n" ++
"open Foo\n" ++
"def main : IO Unit := do\n" ++
"  IO.println \"hello world\"\n" ++
"  IO.println (← ref.get)\n" ++
"  IO.println (← vals.get)\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/ModuleInitModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_ModuleInitModule.class", "lean/test/ModuleInitModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  IO.println "ModuleInitModule.class generated."
