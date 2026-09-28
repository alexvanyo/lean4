import Lean.Compiler.JVM

/-!
Tests character escapes, string length, UTF-8 byte sizing, and byte array conversion on JVM.
Models `tests/compile/char_escape.lean`.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def testLeanSource : String :=
"def badString : String := \"\\x01abc\"\n" ++
"\n" ++
"def runCharEscapeTest : String :=\n" ++
"  let len := badString.length\n" ++
"  let utf8Size := badString.utf8ByteSize\n" ++
"  let ba := badString.toUTF8\n" ++
"  let baSize := ba.size\n" ++
"  let b0 := ba[0]!\n" ++
"  let b1 := ba[1]!\n" ++
"  let b2 := ba[2]!\n" ++
"  let b3 := ba[3]!\n" ++
"  s!\"{len}|{utf8Size}|{baSize}|{b0}|{b1}|{b2}|{b3}\"\n"

def main : IO Unit := do
  IO.FS.createDirAll "lean/test"
  IO.FS.writeFile "lean/test/CharEscapeModule.lean" testLeanSource
  let out ← IO.Process.output {
    cmd := "lean"
    args := #["-R", ".", "--jvm=lean/mod_l_lean_test_CharEscapeModule.class", "lean/test/CharEscapeModule.lean"]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"lean --jvm failed with exit code {out.exitCode}: {out.stderr}"

  -- Emit runner bytecode: calls runCharEscapeTest() and prints result
  let mut cf : ClassFile := {
    className := "lean/test/CharEscapeRunner"
    superClass := "java/lang/Object"
  }
  let (runTestRef, cf') := cf.addMethodRef "lean/mod_l_lean_test_CharEscapeModule" "f_runCharEscapeTest" "()Llean/runtime/LeanObject;"
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

  IO.FS.writeBinFile "lean/test/CharEscapeRunner.class" cf.toByteArray
  IO.println "CharEscapeRunner.class generated."
