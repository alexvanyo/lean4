import Lean.Compiler.JVM

/-!
Tests `Option` construction and pattern matching on the JVM backend across `Nat`, `String`,
primitive scalars (`UInt32`), and nested `Option (Option Nat)`.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def testLeanSource : String :=
"def optNat (x : Nat) : Option Nat := Option.some x\n" ++
"def matchOpt (x : Option Nat) : Nat :=\n" ++
"  match x with\n" ++
"  | none => 42\n" ++
"  | some v => v\n" ++
"def optString (x : String) : Option String := Option.some x\n" ++
"def matchOptString (x : Option String) : String :=\n" ++
"  match x with\n" ++
"  | none => \"none\"\n" ++
"  | some v => v\n" ++
"def optOptNat (x : Option Nat) : Option (Option Nat) := Option.some x\n" ++
"def matchOptOptNat (x : Option (Option Nat)) : Nat :=\n" ++
"  match x with\n" ++
"  | none => 999\n" ++
"  | some none => 888\n" ++
"  | some (some v) => v\n" ++
"def optUInt32 (x : UInt32) : Option UInt32 := Option.some x\n" ++
"def matchOptUInt32 (x : Option UInt32) : UInt32 :=\n" ++
"  match x with\n" ++
"  | none => 123\n" ++
"  | some v => v\n" ++
"def main : IO Unit := do\n" ++
"  IO.println (matchOpt (optNat 100))\n" ++
"  IO.println (matchOpt none)\n" ++
"  IO.println (matchOptString (optString \"hello\"))\n" ++
"  IO.println (matchOptString none)\n" ++
"  IO.println (matchOptOptNat (optOptNat (optNat 777)))\n" ++
"  IO.println (matchOptOptNat (optOptNat none))\n" ++
"  IO.println (matchOptOptNat none)\n" ++
"  IO.println (matchOptUInt32 (optUInt32 99))\n" ++
"  IO.println (matchOptUInt32 none)\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/OptionModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_OptionModule.class", "lean/test/OptionModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr} {out.stdout}"

  IO.println "OptionModule.class generated."
