import Lean.Compiler.JVM

/-!
Tests multi-argument closure over-application on the JVM.
Reproducer for #14969 modeled after `tests/compile/apply_m_overapp.lean`,
verifying that applying 20 arguments to a curried closure chain of arity 1
evaluates without operand stack corruption.
-/

open Lean.Compiler.JVM
open Lean.Compiler.JVM.Opcode

def emitChainClosureClass : IO Unit := do
  let mut cf : ClassFile := {
    className := "lean/test/ChainClosure"
    superClass := "lean/runtime/LeanClosure"
  }

  let (superInitRef, cf') := cf.addMethodRef "lean/runtime/LeanClosure" "<init>" "(I[Llean/runtime/LeanObject;)V"
  let (fieldRef, cf') := cf'.addFieldRef "lean/test/ChainClosure" "remaining" "I"
  let (chainClassRef, cf') := cf'.addClass "lean/test/ChainClosure"
  let (chainInitRef, cf') := cf'.addMethodRef "lean/test/ChainClosure" "<init>" "(I)V"
  let (natOfLongRef, cf') := cf'.addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
  cf := cf'

  -- Field: public int remaining
  let remField : FieldDef := {
    accessFlags := ClassFile.ACC_PUBLIC
    name := "remaining"
    descriptor := "I"
  }
  cf := { cf with fields := cf.fields.push remField }

  -- Constructor: public ChainClosure(int remaining)
  let mut initCode := ByteArray.empty
  initCode := Opcode.aload 0 |>.emit initCode
  initCode := Opcode.iconst_1 |>.emit initCode
  initCode := Opcode.aconst_null |>.emit initCode
  initCode := Opcode.invokespecial superInitRef |>.emit initCode
  initCode := Opcode.aload 0 |>.emit initCode
  initCode := Opcode.iload 1 |>.emit initCode
  initCode := Opcode.putfield fieldRef |>.emit initCode
  initCode := Opcode.return_void |>.emit initCode

  let initMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC
    name := "<init>"
    descriptor := "(I)V"
    maxStack := 3
    maxLocals := 2
    bytecodes := initCode
  }
  cf := { cf with methods := cf.methods.push initMethod }

  -- Method: public LeanObject invokeBody(LeanObject[] args)
  let mut bodyCode := ByteArray.empty
  bodyCode := Opcode.aload 0 |>.emit bodyCode
  bodyCode := Opcode.getfield fieldRef |>.emit bodyCode
  bodyCode := Opcode.iconst_1 |>.emit bodyCode
  bodyCode := Opcode.if_icmpne 10 |>.emit bodyCode
  -- return LeanNat.ofLong(42L)
  bodyCode := Opcode.bipush 42 |>.emit bodyCode
  bodyCode := Opcode.i2l |>.emit bodyCode
  bodyCode := Opcode.invokestatic natOfLongRef |>.emit bodyCode
  bodyCode := Opcode.areturn |>.emit bodyCode
  -- else: return new ChainClosure(remaining - 1)
  bodyCode := Opcode.new chainClassRef |>.emit bodyCode
  bodyCode := Opcode.dup |>.emit bodyCode
  bodyCode := Opcode.aload 0 |>.emit bodyCode
  bodyCode := Opcode.getfield fieldRef |>.emit bodyCode
  bodyCode := Opcode.iconst_1 |>.emit bodyCode
  bodyCode := Opcode.isub |>.emit bodyCode
  bodyCode := Opcode.invokespecial chainInitRef |>.emit bodyCode
  bodyCode := Opcode.areturn |>.emit bodyCode

  let bodyMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC
    name := "invokeBody"
    descriptor := "([Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
    maxStack := 5
    maxLocals := 2
    bytecodes := bodyCode
  }
  cf := { cf with methods := cf.methods.push bodyMethod }

  -- Method: public LeanClosure copyCurried(LeanObject[] newCaptured)
  let mut curriedCode := ByteArray.empty
  curriedCode := Opcode.new chainClassRef |>.emit curriedCode
  curriedCode := Opcode.dup |>.emit curriedCode
  curriedCode := Opcode.aload 0 |>.emit curriedCode
  curriedCode := Opcode.getfield fieldRef |>.emit curriedCode
  curriedCode := Opcode.invokespecial chainInitRef |>.emit curriedCode
  curriedCode := Opcode.areturn |>.emit curriedCode

  let curriedMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC
    name := "copyCurried"
    descriptor := "([Llean/runtime/LeanObject;)Llean/runtime/LeanClosure;"
    maxStack := 4
    maxLocals := 2
    bytecodes := curriedCode
  }
  cf := { cf with methods := cf.methods.push curriedMethod }

  IO.FS.createDirAll "lean/test"
  IO.FS.writeBinFile "lean/test/ChainClosure.class" cf.toByteArray

def main : IO Unit := do
  emitChainClosureClass

  let mut cf : ClassFile := {
    className := "lean/test/ApplyOverappTest"
    superClass := "java/lang/Object"
  }

  let (chainClassRef, cf') := cf.addClass "lean/test/ChainClosure"
  let (chainInitRef, cf') := cf'.addMethodRef "lean/test/ChainClosure" "<init>" "(I)V"
  let (natClassRef, cf') := cf'.addClass "lean/runtime/LeanObject"
  let (natOfLongRef, cf') := cf'.addMethodRef "lean/runtime/LeanNat" "ofLong" "(J)Llean/runtime/LeanNat;"
  let (applyRef, cf') := cf'.addMethodRef "lean/runtime/LeanClosure" "apply" "([Llean/runtime/LeanObject;)Llean/runtime/LeanObject;"
  let (sysOutRef, cf') := cf'.addFieldRef "java/lang/System" "out" "Ljava/io/PrintStream;"
  let (printlnRef, cf') := cf'.addMethodRef "java/io/PrintStream" "println" "(Ljava/lang/Object;)V"
  cf := cf'

  /-
  public static void main(String[] args) {
    ChainClosure c = new ChainClosure(20);
    LeanObject[] arr = new LeanObject[20];
    for (int i = 0; i < 20; i++) arr[i] = LeanNat.ofLong(i + 1);
    LeanObject res = c.apply(arr);
    System.out.println(res); // 42
  }
  -/
  let mut mainCode := ByteArray.empty
  -- c = new ChainClosure(20)
  mainCode := Opcode.new chainClassRef |>.emit mainCode
  mainCode := Opcode.dup |>.emit mainCode
  mainCode := Opcode.bipush 20 |>.emit mainCode
  mainCode := Opcode.invokespecial chainInitRef |>.emit mainCode
  mainCode := Opcode.astore 1 |>.emit mainCode -- local 1 = c

  -- arr = new LeanObject[20]
  mainCode := Opcode.bipush 20 |>.emit mainCode
  mainCode := Opcode.anewarray natClassRef |>.emit mainCode
  mainCode := Opcode.astore 2 |>.emit mainCode -- local 2 = arr

  for i in 0...20 do
    mainCode := Opcode.aload 2 |>.emit mainCode
    mainCode := Opcode.bipush i.toUInt8 |>.emit mainCode
    mainCode := Opcode.bipush (i + 1).toUInt8 |>.emit mainCode
    mainCode := Opcode.i2l |>.emit mainCode
    mainCode := Opcode.invokestatic natOfLongRef |>.emit mainCode
    mainCode := Opcode.aastore |>.emit mainCode

  -- res = c.apply(arr)
  mainCode := Opcode.aload 1 |>.emit mainCode
  mainCode := Opcode.aload 2 |>.emit mainCode
  mainCode := Opcode.invokevirtual applyRef |>.emit mainCode
  mainCode := Opcode.astore 3 |>.emit mainCode -- local 3 = res

  -- System.out.println(res)
  mainCode := Opcode.getstatic sysOutRef |>.emit mainCode
  mainCode := Opcode.aload 3 |>.emit mainCode
  mainCode := Opcode.invokevirtual printlnRef |>.emit mainCode
  mainCode := Opcode.return_void |>.emit mainCode

  let mainMethod : MethodDef := {
    accessFlags := ClassFile.ACC_PUBLIC ||| ClassFile.ACC_STATIC
    name := "main"
    descriptor := "([Ljava/lang/String;)V"
    maxStack := 5
    maxLocals := 4
    bytecodes := mainCode
  }
  cf := { cf with methods := cf.methods.push mainMethod }

  IO.FS.writeBinFile "lean/test/ApplyOverappTest.class" cf.toByteArray
  IO.println "ApplyOverappTest.class generated."
