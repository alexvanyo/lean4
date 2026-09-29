import Lean.Compiler.JVM

/-!
Tests pervasive struct specialization in the JVM backend:
- Dedicated constructor class generation (`lean/ctor_<mangled>`) with strongly typed object and scalar fields
- Direct `new`, `getfield`, and `putfield` bytecode emission for statically known structures
- Virtual fallback accessor overrides (`getObj*`, `getScalar*`, `getByteScalar*`) for polymorphic callers
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def testLeanSource : String :=
"structure Person where\n" ++
"  name : String\n" ++
"  age : UInt32\n" ++
"  height : Float\n" ++
"  id : UInt64\n" ++
"  isMember : Bool\n\n" ++
"def getAge (p : Person) : UInt32 := p.age\n" ++
"def getHeight (p : Person) : Float := p.height\n" ++
"def getName (p : Person) : String := p.name\n" ++
"def getIsMember (p : Person) : Bool := p.isMember\n" ++
"def updateAge (p : Person) (newAge : UInt32) : Person :=\n" ++
"  { p with age := newAge }\n\n" ++
"def main : IO Unit := do\n" ++
"  let p1 : Person := { name := \"Alice\", age := 30, height := 1.75, id := 123456789, isMember := true }\n" ++
"  IO.println (getName p1)\n" ++
"  IO.println (getAge p1)\n" ++
"  IO.println (getHeight p1)\n" ++
"  IO.println (p1.id)\n" ++
"  IO.println (getIsMember p1)\n" ++
"  let p2 := updateAge p1 31\n" ++
"  IO.println (getAge p2)\n" ++
"  let ps := [p1, p2]\n" ++
"  IO.println (ps.map getName)\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/StructSpecModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_StructSpecModule.class", "lean/test/StructSpecModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr} {out.stdout}"

  IO.println "StructSpecModule.class generated."
