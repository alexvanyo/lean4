import Lean.Compiler.JVM

/-!
Tests closures capturing constructor structures and projecting fields within the closure body.
Models `tests/compile/closure_bug3.lean`, verifying structure field preservation across closure captures.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def testLeanSource : String :=
"structure Point where\n" ++
"  x : Nat\n" ++
"  y : Nat\n" ++
"\n" ++
"@[noinline] def makePointScaler (p : Point) : Nat → Nat :=\n" ++
"  fun factor => p.x * factor + p.y\n" ++
"\n" ++
"@[noinline] def createPair (p : Point) : Point × (Nat → Nat) :=\n" ++
"  (p, makePointScaler p)\n" ++
"\n" ++
"def runCtorTest (x y factor : Nat) : Nat :=\n" ++
"  let pair := createPair { x := x, y := y }\n" ++
"  let scaler := pair.2\n" ++
"  scaler factor\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/ClosureCaptureCtorModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_ClosureCaptureCtorModule.class", "lean/test/ClosureCaptureCtorModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  -- Emit runner bytecode: calls runCtorTest(3, 7, 10) and prints result (37)
  let mut cf : ClassFile := {
    className := "lean/test/ClosureCaptureCtorRunner"
    superClass := "java/lang/Object"
  }
  let (natOfLongRef, cf') := cf.addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
  let (runCtorRef, cf') := cf'.addMethodRef "lean/mod_l_lean_test_ClosureCaptureCtorModule" "f_runCtorTest" "(Llean/runtime/LeanObject;Llean/runtime/LeanObject;Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnObjRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"
  cf := cf'

  let mut mainCode := ByteArray.empty
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  -- arg 0: 3
  mainCode := Opcode.iconst_3 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  -- arg 1: 7
  mainCode := Opcode.bipush 7 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  -- arg 2: 10
  mainCode := Opcode.bipush 10 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  -- call f_runCtorTest(3, 7, 10)
  mainCode := Opcode.invokestatic runCtorRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode
  mainCode := Opcode.return_void |>.emit mainCode

  let mainMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "main"
    descriptor := "([Ljava/lang/String;)V"
    maxStack := 5
    maxLocals := 1
    bytecodes := mainCode
  }
  cf := { cf with methods := cf.methods.push mainMethod }

  IO.FS.writeBinFile "lean/test/ClosureCaptureCtorRunner.class" cf.toByteArray
  IO.println "ClosureCaptureCtorRunner.class generated."
