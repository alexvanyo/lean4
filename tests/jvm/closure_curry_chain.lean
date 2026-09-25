import Lean.Compiler.JVM

/-!
Tests compilation and runtime execution of curried closures and partial applications (.pap).
Models `tests/compile/closure_bug2.lean` (#14969), compiling Lean definitions with `@[noinline]`
to JVM bytecode via `lean --jvm` and verifying dynamic closure currying and invocation.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def testLeanSource : String :=
"@[noinline] def makeAdder (x : Nat) : Nat → Nat := fun y => x + y\n" ++
"@[noinline] def makeCurried (x : Nat) : Nat → Nat → Nat := fun y z => x + y + z\n" ++
"@[noinline] def applyOne (f : Nat → Nat) (v : Nat) : Nat := f v\n" ++
"@[noinline] def applyCurriedStep (g : Nat → Nat → Nat) (v1 : Nat) : Nat → Nat := g v1\n" ++
"def runCurryTest (x : Nat) : Nat :=\n" ++
"  let f := makeAdder x\n" ++
"  let g := makeCurried x\n" ++
"  let step := applyCurriedStep g 20\n" ++
"  applyOne f 10 + step 30\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/ClosureCurryModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_ClosureCurryModule.class", "lean/test/ClosureCurryModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  -- Emit runner bytecode: calls runCurryTest(5) and prints result (70)
  let mut cf : ClassFile := {
    className := "lean/test/ClosureCurryRunner"
    superClass := "java/lang/Object"
  }
  let (natOfLongRef, cf') := cf.addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
  let (runCurryRef, cf') := cf'.addMethodRef "lean/mod_l_lean_test_ClosureCurryModule" "f_runCurryTest" "(Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnObjRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"
  cf := cf'

  let mut mainCode := ByteArray.empty
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.iconst_5 |>.emit mainCode
  mainCode := Opcode.i2l |>.emit mainCode
  mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
  mainCode := Opcode.invokestatic runCurryRef |>.emit mainCode
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

  IO.FS.writeBinFile "lean/test/ClosureCurryRunner.class" cf.toByteArray
  IO.println "ClosureCurryRunner.class generated."
