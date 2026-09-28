import Lean.Compiler.JVM

/-!
Tests floating point arithmetic, comparisons, conversions, structures, closures, and formatting on JVM.
Models `tests/compile/float.lean`.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def testLeanSource : String :=
"structure Foo where\n" ++
"  x : Nat\n" ++
"  w : UInt64\n" ++
"  y : Float\n" ++
"  z : Float\n" ++
"\n" ++
"@[noinline] def mkFoo (x : Nat) : Foo :=\n" ++
"  { x := x, w := x.toUInt64, y := x.toFloat / 3.0, z := x.toFloat / 2.0 }\n" ++
"\n" ++
"@[noinline] def fMap (f : Float → Float) (xs : List Float) : List Float :=\n" ++
"  xs.map f\n" ++
"\n" ++
"@[noinline] def testArith : String :=\n" ++
"  let f1 : Float := 1.0\n" ++
"  let f2 : Float := 2.0\n" ++
"  let f3 : Float := 3.0\n" ++
"  let a := f1 + f2\n" ++
"  let b := f2 - f3\n" ++
"  let c := f3 * f2\n" ++
"  let d := f3 / f2\n" ++
"  let pw := (2.0 : Float) ^ (3.0 : Float)\n" ++
"  s!\"{a}|{b}|{c}|{d}|{pw}\"\n" ++
"\n" ++
"@[noinline] def testCmp : String :=\n" ++
"  let f2 : Float := 2.0\n" ++
"  let f3 : Float := 3.0\n" ++
"  let lt1 := decide (f3 < f2)\n" ++
"  let lt2 := decide (f2 < f3)\n" ++
"  let eq1 := (f3 == f2)\n" ++
"  let eq2 := (f2 == f2)\n" ++
"  let le1 := decide (f3 ≤ f2)\n" ++
"  let le2 := decide (f2 ≤ f2)\n" ++
"  s!\"{lt1}|{lt2}|{eq1}|{eq2}|{le1}|{le2}\"\n" ++
"\n" ++
"@[noinline] def testConv : String :=\n" ++
"  let ofInt0 := Float.ofInt 0\n" ++
"  let ofInt42 := Float.ofInt 42\n" ++
"  let ofIntNeg := Float.ofInt (-42)\n" ++
"  let u8 := (256 : Float).toUInt8\n" ++
"  let u16 := (65536 : Float).toUInt16\n" ++
"  let u32 := (4294967296 : Float).toUInt32\n" ++
"  s!\"{ofInt0}|{ofInt42}|{ofIntNeg}|{u8}|{u16}|{u32}\"\n" ++
"\n" ++
"@[noinline] def testSpecial : String :=\n" ++
"  let nanIsNaN := Float.nan.isNaN\n" ++
"  let infIsInf := Float.inf.isInf\n" ++
"  let fin := (1.5 : Float).isFinite\n" ++
"  s!\"{nanIsNaN}|{infIsInf}|{fin}\"\n" ++
"\n" ++
"@[noinline] def testStructAndList : String :=\n" ++
"  let f3 : Float := 3.0\n" ++
"  let foo := mkFoo 7\n" ++
"  let mapped := fMap (fun x => x / 2.0) [3.0, 4.0]\n" ++
"  let bRound := Float.ofBits f3.toBits == f3\n" ++
"  s!\"{foo.y}|{foo.z}|{mapped}|{bRound}\"\n" ++
"\n" ++
"def runFloatTest : String :=\n" ++
"  s!\"{testArith}|{testCmp}|{testConv}|{testSpecial}|{testStructAndList}\"\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/FloatOpsModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_FloatOpsModule.class", "lean/test/FloatOpsModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  -- Emit runner bytecode: calls runFloatTest() and prints result
  let mut cf : ClassFile := {
    className := "lean/test/FloatOpsRunner"
    superClass := "java/lang/Object"
  }
  let (runTestRef, cf') := cf.addMethodRef "lean/mod_l_lean_test_FloatOpsModule" "f_runFloatTest" "()Llean/runtime/LeanObject;"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnObjRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"
  cf := cf'

  let mut mainCode := ByteArray.empty
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.invokestatic runTestRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode
  mainCode := Opcode.return_void |>.emit mainCode

  let mainMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "main"
    descriptor := "([Ljava/lang/String;)V"
    maxStack := 3
    maxLocals := 1
    bytecodes := mainCode
  }
  cf := { cf with methods := cf.methods.push mainMethod }

  IO.FS.writeBinFile "lean/test/FloatOpsRunner.class" cf.toByteArray
  IO.println "FloatOpsRunner.class generated."
