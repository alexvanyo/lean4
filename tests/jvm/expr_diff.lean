import Lean.Compiler.JVM

/-!
Tests symbolic AST differentiation and expression manipulation on JVM.
Models `tests/compile/t2.lean`, verifying mutual recursion, deep pattern matching, and inductive types.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def testLeanSource : String :=
"inductive Expr\n" ++
"| Val : Nat → Expr\n" ++
"| Var : Nat → Expr\n" ++
"| Add : Expr → Expr → Expr\n" ++
"| Mul : Expr → Expr → Expr\n" ++
"\n" ++
"namespace Expr\n" ++
"\n" ++
"def add : Expr → Expr → Expr\n" ++
"| Val n, Val m => Val (n + m)\n" ++
"| Val 0, f     => f\n" ++
"| f,     Val 0 => f\n" ++
"| f,     g     => Add f g\n" ++
"\n" ++
"def mul : Expr → Expr → Expr\n" ++
"| Val n, Val m => Val (n * m)\n" ++
"| Val 0, _     => Val 0\n" ++
"| _,     Val 0 => Val 0\n" ++
"| Val 1, f     => f\n" ++
"| f,     Val 1 => f\n" ++
"| f,     g     => Mul f g\n" ++
"\n" ++
"def d (x : Nat) : Expr → Expr\n" ++
"| Val _   => Val 0\n" ++
"| Var y   => if x == y then Val 1 else Val 0\n" ++
"| Add f g => add (d x f) (d x g)\n" ++
"| Mul f g => add (mul f (d x g)) (mul g (d x f))\n" ++
"\n" ++
"def count : Expr → Nat\n" ++
"| Val _   => 1\n" ++
"| Var _   => 1\n" ++
"| Add f g => 1 + count f + count g\n" ++
"| Mul f g => 1 + count f + count g\n" ++
"\n" ++
"end Expr\n" ++
"\n" ++
"def runDiffTest : Nat :=\n" ++
"  let x := Expr.Var 0\n" ++
"  let f := Expr.Add (Expr.Mul x x) (Expr.Mul x (Expr.Val 3))\n" ++
"  let df := Expr.d 0 f\n" ++
"  Expr.count df\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/ExprDiffModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_ExprDiffModule.class", "lean/test/ExprDiffModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  -- Emit runner bytecode: calls runDiffTest() and prints result (5)
  let mut cf : ClassFile := {
    className := "lean/test/ExprDiffRunner"
    superClass := "java/lang/Object"
  }
  let (runDiffRef, cf') := cf.addMethodRef "lean/mod_l_lean_test_ExprDiffModule" "f_runDiffTest" "()Llean/runtime/LeanObject;"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnObjRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"
  cf := cf'

  let mut mainCode := ByteArray.empty
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.invokestatic runDiffRef |>.emit mainCode
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

  IO.FS.writeBinFile "lean/test/ExprDiffRunner.class" cf.toByteArray
  IO.println "ExprDiffRunner.class generated."
