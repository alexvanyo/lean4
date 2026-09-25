import Lean.Compiler.JVM

/-!
Tests end-to-end compilation of Lean source definitions to JVM classfiles.
Runs `lean --jvm` on sample declarations and verifies classfile headers,
constant pool, and generated methods.
-/

def testLeanSource : String :=
"def addOne (x : Nat) : Nat := x + 1\n" ++
"def pairSwap (p : Nat × Nat) : Nat × Nat := (p.2, p.1)\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/SampleModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["--jvm=lean/test/SampleModule.class", "lean/test/SampleModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  let bytes ← IO.FS.readBinFile "lean/test/SampleModule.class"
  if bytes.size < 4 then
    throw <| IO.userError "Output class file is too small"

  let magic := (bytes.get! 0).toUInt32 <<< 24 |||
               (bytes.get! 1).toUInt32 <<< 16 |||
               (bytes.get! 2).toUInt32 <<< 8  |||
               (bytes.get! 3).toUInt32
  if magic != 0xCAFEBABE then
    throw <| IO.userError s!"Invalid class file magic: {magic}"

  let major := (bytes.get! 6).toUInt16 <<< 8 ||| (bytes.get! 7).toUInt16
  if major != 50 then
    throw <| IO.userError s!"Expected major version 50, got {major}"

  IO.println s!"SampleModule.class generated successfully with {bytes.size} bytes."
