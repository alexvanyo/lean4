import Lean.Compiler.JVM

/-!
Tests lazy evaluation and streams via recursive `Thunk` execution on JVM.
Models `tests/compile/lazylist.lean`, verifying memoized thunk application and lazy stream iteration.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def testLeanSource : String :=
"inductive StreamNat where\n" ++
"| nil : StreamNat\n" ++
"| cons : Nat → Thunk StreamNat → StreamNat\n" ++
"deriving Inhabited\n" ++
"\n" ++
"namespace StreamNat\n" ++
"\n" ++
"partial def fibs (a b : Nat) : StreamNat :=\n" ++
"  cons a (Thunk.mk fun _ => fibs b (a + b))\n" ++
"\n" ++
"def nth (n : Nat) (s : StreamNat) : Nat :=\n" ++
"  match n with\n" ++
"  | 0 =>\n" ++
"    match s with\n" ++
"    | nil => 0\n" ++
"    | cons hd _ => hd\n" ++
"  | n + 1 =>\n" ++
"    match s with\n" ++
"    | nil => 0\n" ++
"    | cons _ tl => nth n tl.get\n" ++
"\n" ++
"end StreamNat\n" ++
"\n" ++
"def runLazyFib : Nat :=\n" ++
"  let s := StreamNat.fibs 0 1\n" ++
"  StreamNat.nth 7 s\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/LazyListModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_LazyListModule.class", "lean/test/LazyListModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  -- Emit runner bytecode: calls runLazyFib() and prints result (13)
  let mut cf : ClassFile := {
    className := "lean/test/LazyListRunner"
    superClass := "java/lang/Object"
  }
  let (runFibRef, cf') := cf.addMethodRef "lean/mod_l_lean_test_LazyListModule" "f_runLazyFib" "()Llean/runtime/LeanObject;"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnObjRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"
  cf := cf'

  let mut mainCode := ByteArray.empty
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.invokestatic runFibRef |>.emit mainCode
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

  IO.FS.writeBinFile "lean/test/LazyListRunner.class" cf.toByteArray
  IO.println "LazyListRunner.class generated."
