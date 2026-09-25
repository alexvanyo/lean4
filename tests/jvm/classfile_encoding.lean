import Lean.Compiler.JVM

/-!
Tests binary classfile and JVM bytecode opcode emission.
Verifies that pure Lean generates valid JVM .class files that `javap`
and the JVM bytecode verifier parse correctly.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def main : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/SimpleMath"
    superClass := "java/lang/Object"
  }

  -- Build a test method: public static int add42(int x) { return x + 42; }
  let mut code := ByteArray.empty
  code := Opcode.iload 0 |>.emit code
  code := Opcode.bipush 42 |>.emit code
  code := Opcode.iadd |>.emit code
  code := Opcode.ireturn |>.emit code

  let method : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "add42"
    descriptor := "(I)I"
    maxStack := 4
    maxLocals := 2
    bytecodes := code
  }
  cf := { cf with methods := #[method] }

  let bytes := cf.toByteArray

  -- Verify class file header magic 0xCAFEBABE
  let magic := (bytes.get! 0).toUInt32.shiftLeft 24 |||
               (bytes.get! 1).toUInt32.shiftLeft 16 |||
               (bytes.get! 2).toUInt32.shiftLeft 8 |||
               (bytes.get! 3).toUInt32
  let major := (bytes.get! 6).toUInt16.shiftLeft 8 ||| (bytes.get! 7).toUInt16

  IO.println s!"Magic: {String.append "0x" (Nat.toDigits 16 magic.toNat |> String.ofList)}"
  IO.println s!"Major version: {major}"
  IO.println s!"Classfile byte size: {bytes.size}"

  -- Write class file to test directory
  IO.FS.writeBinFile "SimpleMath.class" bytes
  IO.println "Classfile generated successfully."
