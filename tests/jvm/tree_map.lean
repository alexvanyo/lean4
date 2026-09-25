import Lean.Compiler.JVM

/-!
Tests balanced inductive tree map insertion, deletion, and lookup on JVM.
Models `tests/compile/rbmap_library.lean`, testing inductive structural sharing, traversal, and cases branching.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def testLeanSource : String :=
"inductive Tree where\n" ++
"| leaf : Tree\n" ++
"| node : Nat → Nat → Tree → Tree → Tree\n" ++
"\n" ++
"namespace Tree\n" ++
"\n" ++
"def insert (k v : Nat) : Tree → Tree\n" ++
"| leaf => node k v leaf leaf\n" ++
"| node k' v' left right =>\n" ++
"  if k < k' then\n" ++
"    node k' v' (insert k v left) right\n" ++
"  else if k > k' then\n" ++
"    node k' v' left (insert k v right)\n" ++
"  else\n" ++
"    node k v left right\n" ++
"\n" ++
"def find? (k : Nat) : Tree → Option Nat\n" ++
"| leaf => none\n" ++
"| node k' v' left right =>\n" ++
"  if k < k' then\n" ++
"    find? k left\n" ++
"  else if k > k' then\n" ++
"    find? k right\n" ++
"  else\n" ++
"    some v'\n" ++
"\n" ++
"def minKey : Tree → Option (Nat × Nat)\n" ++
"| leaf => none\n" ++
"| node k v leaf _ => some (k, v)\n" ++
"| node _ _ left _ => minKey left\n" ++
"\n" ++
"def erase (k : Nat) : Tree → Tree\n" ++
"| leaf => leaf\n" ++
"| node k' v' left right =>\n" ++
"  if k < k' then\n" ++
"    node k' v' (erase k left) right\n" ++
"  else if k > k' then\n" ++
"    node k' v' left (erase k right)\n" ++
"  else\n" ++
"    match left, right with\n" ++
"    | leaf, r => r\n" ++
"    | l, leaf => l\n" ++
"    | l, r =>\n" ++
"      match minKey r with\n" ++
"      | none => l\n" ++
"      | some (mk, mv) => node mk mv l (erase mk r)\n" ++
"\n" ++
"def size : Tree → Nat\n" ++
"| leaf => 0\n" ++
"| node _ _ left right => 1 + size left + size right\n" ++
"\n" ++
"end Tree\n" ++
"\n" ++
"def runTreeTest : Nat :=\n" ++
"  let t := Tree.leaf\n" ++
"  let t := t.insert 5 50\n" ++
"  let t := t.insert 2 20\n" ++
"  let t := t.insert 8 80\n" ++
"  let t := t.insert 1 10\n" ++
"  let t := t.insert 3 30\n" ++
"  let t := t.erase 2\n" ++
"  let v := (t.find? 3).getD 0\n" ++
"  t.size * 100 + v\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/TreeMapModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_TreeMapModule.class", "lean/test/TreeMapModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  -- Emit runner bytecode: calls runTreeTest() and prints result (430)
  let mut cf : ClassFile := {
    className := "lean/test/TreeMapRunner"
    superClass := "java/lang/Object"
  }
  let (runTreeRef, cf') := cf.addMethodRef "lean/mod_l_lean_test_TreeMapModule" "f_runTreeTest" "()Llean/runtime/LeanObject;"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnObjRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"
  cf := cf'

  let mut mainCode := ByteArray.empty
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.invokestatic runTreeRef |>.emit mainCode
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

  IO.FS.writeBinFile "lean/test/TreeMapRunner.class" cf.toByteArray
  IO.println "TreeMapRunner.class generated."
