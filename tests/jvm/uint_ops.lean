import Lean.Compiler.JVM

/-!
Tests UInt8, UInt16, UInt32, UInt64, and USize arithmetic, bitwise operations, and boundary overflow on JVM.
-/

def testLeanSource : String :=
"def main : IO Unit := do\n" ++
"  let u8_a : UInt8 := 200\n" ++
"  let u8_b : UInt8 := 100\n" ++
"  IO.println (u8_a + u8_b)\n" ++              -- 44 (wrapping)
"  IO.println (u8_a - u8_b)\n" ++              -- 100
"  IO.println (u8_b - u8_a)\n" ++              -- 156 (wrapping)
"  IO.println (u8_a * 2)\n" ++                 -- 144 (wrapping)
"  IO.println (u8_a / 10)\n" ++                -- 20
"  IO.println (u8_a % 30)\n" ++                -- 20
"  IO.println (u8_a &&& u8_b)\n" ++            -- 64
"  IO.println (u8_a ||| u8_b)\n" ++            -- 236
"  IO.println (u8_a ^^^ u8_b)\n" ++            -- 172
"  IO.println (u8_b <<< 1)\n" ++               -- 200
"  IO.println (u8_a >>> 1)\n" ++               -- 100
"  IO.println (decide (u8_b < u8_a))\n" ++     -- true
"\n" ++
"  let u16_a : UInt16 := 40000\n" ++
"  let u16_b : UInt16 := 30000\n" ++
"  IO.println (u16_a + u16_b)\n" ++            -- 4464
"  IO.println (u16_a - u16_b)\n" ++            -- 10000
"\n" ++
"  let u32_a : UInt32 := 3000000000\n" ++
"  let u32_b : UInt32 := 2000000000\n" ++
"  IO.println (u32_a + u32_b)\n" ++            -- 705032704
"  IO.println (u32_a - u32_b)\n" ++            -- 1000000000\n" ++
"  IO.println (decide (u32_b < u32_a))\n" ++   -- true
"\n" ++
"  let u64_max : UInt64 := 18446744073709551615\n" ++ -- 2^64 - 1
"  IO.println (u64_max + 1)\n" ++              -- 0
"  IO.println (u64_max - 1)\n" ++              -- 18446744073709551614
"  IO.println (decide (u64_max.toNat > 0))\n" ++ -- true
"  IO.println (u64_max.toNat == 18446744073709551615)\n" -- true

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/UIntOpsModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_UIntOpsModule.class", "lean/test/UIntOpsModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  IO.println "UIntOpsModule.class generated."
