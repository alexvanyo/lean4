import Lean.Compiler.JVM

/-!
Tests concatenating and measuring large lists on JVM.
Models `tests/compile/append.lean`, stress-testing tail-recursive list construction and traversal.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def testLeanSource : String :=
"def runAppend : Nat :=\n" ++
"  let ys1 := List.replicate 1000 1\n" ++
"  let ys2 := List.replicate 1000 2\n" ++
"  (ys1 ++ ys2).length\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/ListAppendModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_ListAppendModule.class", "lean/test/ListAppendModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  -- Emit runner bytecode: calls runAppend() and prints result (2000)
  let mut cf : ClassFile := {
    className := "lean/test/ListAppendRunner"
    superClass := "java/lang/Object"
  }
  let (runAppendRef, cf') := cf.addMethodRef "lean/mod_l_lean_test_ListAppendModule" "f_runAppend" "()Llean/runtime/LeanObject;"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnObjRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"
  cf := cf'

  let mut mainCode := ByteArray.empty
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.invokestatic runAppendRef |>.emit mainCode
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

  IO.FS.writeBinFile "lean/test/ListAppendRunner.class" cf.toByteArray
  IO.println "ListAppendRunner.class generated."
