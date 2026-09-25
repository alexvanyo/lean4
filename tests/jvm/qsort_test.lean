import Lean.Compiler.JVM

/-!
Tests polymorphic array quicksort with custom comparator on JVM.
Models `tests/compile/qsortBadLt.lean`, exercising array element swapping, indexing, and comparator closures.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def testLeanSource : String :=
"def runQSort : Array Nat :=\n" ++
"  let xs := #[5, 3, 4, 1, 2]\n" ++
"  xs.qsort (fun a b => a < b)\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/QSortModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_QSortModule.class", "lean/test/QSortModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  -- Emit runner bytecode: calls runQSort() and prints result (#[1, 2, 3, 4, 5])
  let mut cf : ClassFile := {
    className := "lean/test/QSortRunner"
    superClass := "java/lang/Object"
  }
  let (runQSortRef, cf') := cf.addMethodRef "lean/mod_l_lean_test_QSortModule" "f_runQSort" "()Llean/runtime/LeanObject;"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnObjRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"
  cf := cf'

  let mut mainCode := ByteArray.empty
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.invokestatic runQSortRef |>.emit mainCode
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

  IO.FS.writeBinFile "lean/test/QSortRunner.class" cf.toByteArray
  IO.println "QSortRunner.class generated."
