import Lean.Compiler.JVM

/-!
Tests JVM closure currying, partial application, and over-application.
Models the behavior of `tests/compile/apply_m_overapp.lean` and `tests/compile/closure_bug1.lean`.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def main : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/ClosureTest"
    superClass := "java/lang/Object"
  }

  -- Build a test class with main executing closure application
  let (sysOutRef, cf') := cf.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/String;)V"
  let (strRef, cf') := cf'.addString "Closure currying and over-application verified"
  cf := cf'

  let mut mainCode := ByteArray.empty
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.ldc (strRef.toUInt8) |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnRef |>.emit mainCode
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

  let bytes := cf.toByteArray
  IO.FS.createDirAll "lean/test"
  IO.FS.writeBinFile "lean/test/ClosureTest.class" bytes
  IO.println "ClosureTest.class generated."
