import Lean.Compiler.JVM

/-!
Tests allocating closures inside recursive/loop functions on the JVM.
Models `tests/compile/closure_bug4.lean`, ensuring stack frames and local variables are properly managed across iterations.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def testLeanSource : String :=
"@[noinline] def applyClosure (f : Nat → Nat) (v : Nat) : Nat := f v\n" ++
"\n" ++
"@[noinline] def loopWithClosures (n : Nat) (acc : Nat) : Nat :=\n" ++
"  match n with\n" ++
"  | 0 => acc\n" ++
"  | n + 1 =>\n" ++
"    let multiplier := fun x => x * 2 + 1\n" ++
"    let res := applyClosure multiplier acc\n" ++
"    loopWithClosures n res\n" ++
"\n" ++
"def runLoopTest (n acc : Nat) : Nat :=\n" ++
"  loopWithClosures n acc\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/ClosureInLoopModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_ClosureInLoopModule.class", "lean/test/ClosureInLoopModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  -- Emit runner bytecode: calls runLoopTest(3, 1) and prints result (15)
  let mut cf : ClassFile := {
    className := "lean/test/ClosureInLoopRunner"
    superClass := "java/lang/Object"
  }
  let (natOfLongRef, cf') := cf.addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
  let (runLoopRef, cf') := cf'.addMethodRef "lean/mod_l_lean_test_ClosureInLoopModule" "f_runLoopTest" "(Llean/runtime/LeanObject;Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnObjRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"
  cf := cf'

  let mut mainCode := ByteArray.empty
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  -- arg 0: n = 3
  mainCode := Opcode.iconst_3 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  -- arg 1: acc = 1
  mainCode := Opcode.iconst_1 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  -- call f_runLoopTest(3, 1)
  mainCode := Opcode.invokestatic runLoopRef |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnObjRef |>.emit mainCode
  mainCode := Opcode.return_void |>.emit mainCode

  let mainMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "main"
    descriptor := "([Ljava/lang/String;)V"
    maxStack := 4
    maxLocals := 1
    bytecodes := mainCode
  }
  cf := { cf with methods := cf.methods.push mainMethod }

  IO.FS.writeBinFile "lean/test/ClosureInLoopRunner.class" cf.toByteArray
  IO.println "ClosureInLoopRunner.class generated."
