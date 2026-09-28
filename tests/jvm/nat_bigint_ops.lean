import Lean.Compiler.JVM

/-!
Tests LeanNat arbitrary-precision BigInteger arithmetic, boundary overflow, and comparisons on JVM.
-/

def testLeanSource : String :=
"def main : IO Unit := do\n" ++
"  let a : Nat := 18446744073709551616\n" ++ -- 2^64
"  let b : Nat := 18446744073709551617\n" ++ -- 2^64 + 1
"  let small : Nat := 42\n" ++
"  IO.println (a + b)\n" ++        -- 36893488147419103233
"  IO.println (b - a)\n" ++        -- 1
"  IO.println (a - b)\n" ++        -- 0
"  IO.println (b - small)\n" ++    -- 18446744073709551575
"  IO.println (a * 2)\n" ++        -- 36893488147419103232
"  IO.println (a / 2)\n" ++        -- 9223372036854775808
"  IO.println (b % a)\n" ++        -- 1
"  IO.println (decide (a < b))\n" ++        -- true
"  IO.println (decide (b < a))\n" ++        -- false
"  IO.println (decide (a <= b))\n" ++       -- true
"  IO.println (decide (b <= a))\n" ++       -- false
"  IO.println (decide (small < a))\n" ++    -- true
"  IO.println (decide (a < small))\n" ++    -- false
"  IO.println (a == a)\n" ++       -- true
"  IO.println (a == b)\n"          -- false

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/NatBigIntOpsModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_NatBigIntOpsModule.class", "lean/test/NatBigIntOpsModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  IO.println "NatBigIntOpsModule.class generated."
