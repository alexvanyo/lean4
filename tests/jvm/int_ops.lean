import Lean.Compiler.JVM

/-!
Tests Int operations (small, negative, BigInteger, natAbs, arithmetic, comparison) on JVM.
-/

def testLeanSource : String :=
"def main : IO Unit := do\n" ++
"  let a : Int := -42\n" ++
"  let b : Int := 10\n" ++
"  let c : Int := 18446744073709551616\n" ++  -- 2^64
"  let d : Int := -18446744073709551616\n" ++
"  IO.println (a + b)\n" ++        -- -32
"  IO.println (a - b)\n" ++        -- -52
"  IO.println (b - a)\n" ++        -- 52
"  IO.println (a * b)\n" ++        -- -420
"  IO.println (c + d)\n" ++        -- 0
"  IO.println (c + 1)\n" ++        -- 18446744073709551617
"  IO.println (d - 1)\n" ++        -- -18446744073709551617
"  IO.println a.natAbs\n" ++       -- 42
"  IO.println c.natAbs\n" ++       -- 18446744073709551616
"  IO.println d.natAbs\n" ++       -- 18446744073709551616
"  IO.println (-a)\n" ++           -- 42
"  IO.println (decide (a < b))\n" ++        -- true
"  IO.println (decide (b < a))\n" ++        -- false
"  IO.println (decide (a <= b))\n" ++       -- true
"  IO.println (decide (b <= a))\n" ++       -- false
"  IO.println (decide (d < c))\n" ++        -- true
"  IO.println (a == -42)\n" ++     -- true
"  IO.println (a == b)\n"          -- false

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/IntOpsModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_IntOpsModule.class", "lean/test/IntOpsModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  IO.println "IntOpsModule.class generated."
