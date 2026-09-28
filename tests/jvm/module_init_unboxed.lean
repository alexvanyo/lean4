import Lean.Compiler.JVM

/-!
Tests module initialization of unboxed / primitive scalar types on JVM.
Models `tests/compile/initUnboxed.lean`.
-/

def testLeanSource : String :=
"initialize test : UInt64 ← pure 0\n" ++
"initialize testb : Bool ← pure false\n" ++
"initialize testu : USize ← pure 1\n" ++
"initialize testf : Float ← pure 0.5\n" ++
"initialize test32 : UInt32 ← pure 16\n" ++
"\n" ++
"def main : IO Unit := do\n" ++
"  IO.println test\n" ++
"  IO.println testb\n" ++
"  IO.println testu\n" ++
"  IO.println testf\n" ++
"  IO.println test32\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/ModuleInitUnboxedModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_ModuleInitUnboxedModule.class", "lean/test/ModuleInitUnboxedModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  IO.println "ModuleInitUnboxedModule.class generated."
