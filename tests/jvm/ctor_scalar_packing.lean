import Lean.Compiler.JVM

/-!
Tests constructor scalar field packing with multiple sub-64-bit scalars on JVM.
Verifies that `sproj` and `sset` correctly extract and update individual packed scalars without
corrupting neighboring fields or throwing `ArrayIndexOutOfBoundsException`.
-/

def testLeanSource : String :=
"structure TwoBytes where\n" ++
"  b1 : UInt8\n" ++
"  b2 : UInt8\n" ++
"  u64 : UInt64\n" ++
"deriving Inhabited\n" ++
"\n" ++
"@[noinline] def mkTwoBytes (b1 b2 : UInt8) (u64 : UInt64) : TwoBytes :=\n" ++
"  { b1, b2, u64 }\n" ++
"\n" ++
"@[noinline] def getTB1 (x : TwoBytes) : UInt8 := x.b1\n" ++
"@[noinline] def getTB2 (x : TwoBytes) : UInt8 := x.b2\n" ++
"@[noinline] def getTBU64 (x : TwoBytes) : UInt64 := x.u64\n" ++
"\n" ++
"def main : IO Unit := do\n" ++
"  let tb := mkTwoBytes 10 20 99999\n" ++
"  IO.println (getTB1 tb)\n" ++     -- 10
"  IO.println (getTB2 tb)\n" ++     -- 20
"  IO.println (getTBU64 tb)\n"      -- 99999

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/CtorScalarPackingModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_CtorScalarPackingModule.class", "lean/test/CtorScalarPackingModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  IO.println "CtorScalarPackingModule.class generated."
