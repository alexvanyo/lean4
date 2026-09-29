/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Lean FRO, LLC
-/
module

prelude
public import Init.Data.ByteArray
public import Init.Data.UInt.Basic
public import Init.Data.UInt.Bitwise
import Init.While

public section

namespace Lean.Compiler.JVM

/--
JVM Bytecode Instructions.
Covers instructions used for compiling Lean LCNF code to JVM bytecode.
-/
inductive Opcode where
  | nop
  | aconst_null
  | iconst_m1
  | iconst_0
  | iconst_1
  | iconst_2
  | iconst_3
  | iconst_4
  | iconst_5
  | lconst_0
  | lconst_1
  | bipush (byte : UInt8)
  | sipush (val : UInt16)
  | ldc (cpIdx : UInt8)
  | ldc_w (cpIdx : UInt16)
  | ldc2_w (cpIdx : UInt16)
  | iload (slot : UInt8)
  | lload (slot : UInt8)
  | fload (slot : UInt8)
  | dload (slot : UInt8)
  | aload (slot : UInt8)
  | iaload
  | laload
  | aaload
  | istore (slot : UInt8)
  | lstore (slot : UInt8)
  | fstore (slot : UInt8)
  | dstore (slot : UInt8)
  | astore (slot : UInt8)
  | iastore
  | lastore
  | aastore
  | pop
  | dup
  | dup2
  | swap
  | iadd
  | ladd
  | isub
  | lsub
  | imul
  | lmul
  | idiv
  | ldiv
  | irem
  | lrem
  | iand
  | land
  | ior
  | lor
  | ixor
  | lxor
  | ishl
  | lshl
  | ishr
  | lshr
  | iushr
  | lushr
  | i2l
  | l2i
  | lcmp
  | ifeq (target : UInt16)
  | ifne (target : UInt16)
  | iflt (target : UInt16)
  | ifge (target : UInt16)
  | ifgt (target : UInt16)
  | ifle (target : UInt16)
  | if_icmpeq (target : UInt16)
  | if_icmpne (target : UInt16)
  | if_icmplt (target : UInt16)
  | if_icmpge (target : UInt16)
  | if_icmpgt (target : UInt16)
  | if_icmple (target : UInt16)
  | if_acmpeq (target : UInt16)
  | if_acmpne (target : UInt16)
  | ifnull (target : UInt16)
  | ifnonnull (target : UInt16)
  | goto (target : UInt16)
  | tableswitch (low high : UInt32) (defOffset : UInt32) (offsets : Array UInt32)
  | ireturn
  | lreturn
  | freturn
  | dreturn
  | areturn
  | return_void
  | getstatic (cpIdx : UInt16)
  | putstatic (cpIdx : UInt16)
  | getfield (cpIdx : UInt16)
  | putfield (cpIdx : UInt16)
  | invokevirtual (cpIdx : UInt16)
  | invokespecial (cpIdx : UInt16)
  | invokestatic (cpIdx : UInt16)
  | new (cpIdx : UInt16)
  | newarray (atype : UInt8)
  | anewarray (cpIdx : UInt16)
  | arraylength
  | checkcast (cpIdx : UInt16)
  | instanceof (cpIdx : UInt16)
  | invokedynamic (cpIdx : UInt16)
  deriving Inhabited

namespace Opcode

def writeU8 (ba : ByteArray) (b : UInt8) : ByteArray :=
  ba.push b

def writeU16 (ba : ByteArray) (v : UInt16) : ByteArray :=
  ba.push (v >>> 8).toUInt8 |>.push v.toUInt8

def writeU32 (ba : ByteArray) (v : UInt32) : ByteArray :=
  ba.push (v >>> 24).toUInt8
    |>.push (v >>> 16).toUInt8
    |>.push (v >>> 8).toUInt8
    |>.push v.toUInt8

/--
Serializes a single JVM bytecode instruction into a ByteArray.
-/
def emit (ba : ByteArray) (op : Opcode) : ByteArray :=
  match op with
  | .nop => writeU8 ba 0x00
  | .aconst_null => writeU8 ba 0x01
  | .iconst_m1 => writeU8 ba 0x02
  | .iconst_0 => writeU8 ba 0x03
  | .iconst_1 => writeU8 ba 0x04
  | .iconst_2 => writeU8 ba 0x05
  | .iconst_3 => writeU8 ba 0x06
  | .iconst_4 => writeU8 ba 0x07
  | .iconst_5 => writeU8 ba 0x08
  | .lconst_0 => writeU8 ba 0x09
  | .lconst_1 => writeU8 ba 0x0a
  | .bipush b => writeU8 (writeU8 ba 0x10) b
  | .sipush v => writeU16 (writeU8 ba 0x11) v
  | .ldc idx => writeU8 (writeU8 ba 0x12) idx
  | .ldc_w idx => writeU16 (writeU8 ba 0x13) idx
  | .ldc2_w idx => writeU16 (writeU8 ba 0x14) idx
  | .iload slot =>
    if slot == 0 then writeU8 ba 0x1a
    else if slot == 1 then writeU8 ba 0x1b
    else if slot == 2 then writeU8 ba 0x1c
    else if slot == 3 then writeU8 ba 0x1d
    else writeU8 (writeU8 ba 0x15) slot
  | .lload slot =>
    if slot == 0 then writeU8 ba 0x1e
    else if slot == 1 then writeU8 ba 0x1f
    else if slot == 2 then writeU8 ba 0x20
    else if slot == 3 then writeU8 ba 0x21
    else writeU8 (writeU8 ba 0x16) slot
  | .fload slot =>
    if slot == 0 then writeU8 ba 0x22
    else if slot == 1 then writeU8 ba 0x23
    else if slot == 2 then writeU8 ba 0x24
    else if slot == 3 then writeU8 ba 0x25
    else writeU8 (writeU8 ba 0x17) slot
  | .dload slot =>
    if slot == 0 then writeU8 ba 0x26
    else if slot == 1 then writeU8 ba 0x27
    else if slot == 2 then writeU8 ba 0x28
    else if slot == 3 then writeU8 ba 0x29
    else writeU8 (writeU8 ba 0x18) slot
  | .aload slot =>
    if slot == 0 then writeU8 ba 0x2a
    else if slot == 1 then writeU8 ba 0x2b
    else if slot == 2 then writeU8 ba 0x2c
    else if slot == 3 then writeU8 ba 0x2d
    else writeU8 (writeU8 ba 0x19) slot
  | .iaload => writeU8 ba 0x2e
  | .laload => writeU8 ba 0x2f
  | .aaload => writeU8 ba 0x32
  | .istore slot =>
    if slot == 0 then writeU8 ba 0x3b
    else if slot == 1 then writeU8 ba 0x3c
    else if slot == 2 then writeU8 ba 0x3d
    else if slot == 3 then writeU8 ba 0x3e
    else writeU8 (writeU8 ba 0x36) slot
  | .lstore slot =>
    if slot == 0 then writeU8 ba 0x3f
    else if slot == 1 then writeU8 ba 0x40
    else if slot == 2 then writeU8 ba 0x41
    else if slot == 3 then writeU8 ba 0x42
    else writeU8 (writeU8 ba 0x37) slot
  | .fstore slot =>
    if slot == 0 then writeU8 ba 0x43
    else if slot == 1 then writeU8 ba 0x44
    else if slot == 2 then writeU8 ba 0x45
    else if slot == 3 then writeU8 ba 0x46
    else writeU8 (writeU8 ba 0x38) slot
  | .dstore slot =>
    if slot == 0 then writeU8 ba 0x47
    else if slot == 1 then writeU8 ba 0x48
    else if slot == 2 then writeU8 ba 0x49
    else if slot == 3 then writeU8 ba 0x4a
    else writeU8 (writeU8 ba 0x39) slot
  | .astore slot =>
    if slot == 0 then writeU8 ba 0x4b
    else if slot == 1 then writeU8 ba 0x4c
    else if slot == 2 then writeU8 ba 0x4d
    else if slot == 3 then writeU8 ba 0x4e
    else writeU8 (writeU8 ba 0x3a) slot
  | .iastore => writeU8 ba 0x4f
  | .lastore => writeU8 ba 0x50
  | .aastore => writeU8 ba 0x53
  | .pop => writeU8 ba 0x57
  | .dup => writeU8 ba 0x59
  | .dup2 => writeU8 ba 0x5c
  | .swap => writeU8 ba 0x5f
  | .iadd => writeU8 ba 0x60
  | .ladd => writeU8 ba 0x61
  | .isub => writeU8 ba 0x64
  | .lsub => writeU8 ba 0x65
  | .imul => writeU8 ba 0x68
  | .lmul => writeU8 ba 0x69
  | .idiv => writeU8 ba 0x6c
  | .ldiv => writeU8 ba 0x6d
  | .irem => writeU8 ba 0x70
  | .lrem => writeU8 ba 0x71
  | .iand => writeU8 ba 0x7e
  | .land => writeU8 ba 0x7f
  | .ior => writeU8 ba 0x80
  | .lor => writeU8 ba 0x81
  | .ixor => writeU8 ba 0x82
  | .lxor => writeU8 ba 0x83
  | .ishl => writeU8 ba 0x78
  | .lshl => writeU8 ba 0x79
  | .ishr => writeU8 ba 0x7a
  | .lshr => writeU8 ba 0x7b
  | .iushr => writeU8 ba 0x7c
  | .lushr => writeU8 ba 0x7d
  | .i2l => writeU8 ba 0x85
  | .l2i => writeU8 ba 0x88
  | .lcmp => writeU8 ba 0x94
  | .ifeq target => writeU16 (writeU8 ba 0x99) target
  | .ifne target => writeU16 (writeU8 ba 0x9a) target
  | .iflt target => writeU16 (writeU8 ba 0x9b) target
  | .ifge target => writeU16 (writeU8 ba 0x9c) target
  | .ifgt target => writeU16 (writeU8 ba 0x9d) target
  | .ifle target => writeU16 (writeU8 ba 0x9e) target
  | .if_icmpeq target => writeU16 (writeU8 ba 0x9f) target
  | .if_icmpne target => writeU16 (writeU8 ba 0xa0) target
  | .if_icmplt target => writeU16 (writeU8 ba 0xa1) target
  | .if_icmpge target => writeU16 (writeU8 ba 0xa2) target
  | .if_icmpgt target => writeU16 (writeU8 ba 0xa3) target
  | .if_icmple target => writeU16 (writeU8 ba 0xa4) target
  | .if_acmpeq target => writeU16 (writeU8 ba 0xa5) target
  | .if_acmpne target => writeU16 (writeU8 ba 0xa6) target
  | .ifnull target => writeU16 (writeU8 ba 0xc6) target
  | .ifnonnull target => writeU16 (writeU8 ba 0xc7) target
  | .goto target => writeU16 (writeU8 ba 0xa7) target
  | .tableswitch low high defOffset offsets =>
    Id.run do
      let mut ba := writeU8 ba 0xaa
      let pad := (4 - (ba.size % 4)) % 4
      let mut p := 0
      while p < pad do
        ba := writeU8 ba 0
        p := p + 1
      ba := writeU32 ba defOffset
      ba := writeU32 ba low
      ba := writeU32 ba high
      for off in offsets do
        ba := writeU32 ba off
      return ba
  | .ireturn => writeU8 ba 0xac
  | .lreturn => writeU8 ba 0xad
  | .freturn => writeU8 ba 0xae
  | .dreturn => writeU8 ba 0xaf
  | .areturn => writeU8 ba 0xb0
  | .return_void => writeU8 ba 0xb1
  | .getstatic idx => writeU16 (writeU8 ba 0xb2) idx
  | .putstatic idx => writeU16 (writeU8 ba 0xb3) idx
  | .getfield idx => writeU16 (writeU8 ba 0xb4) idx
  | .putfield idx => writeU16 (writeU8 ba 0xb5) idx
  | .invokevirtual idx => writeU16 (writeU8 ba 0xb6) idx
  | .invokespecial idx => writeU16 (writeU8 ba 0xb7) idx
  | .invokestatic idx => writeU16 (writeU8 ba 0xb8) idx
  | .new idx => writeU16 (writeU8 ba 0xbb) idx
  | .newarray atype => writeU8 (writeU8 ba 0xbc) atype
  | .anewarray idx => writeU16 (writeU8 ba 0xbd) idx
  | .arraylength => writeU8 ba 0xbe
  | .checkcast idx => writeU16 (writeU8 ba 0xc0) idx
  | .instanceof idx => writeU16 (writeU8 ba 0xc1) idx
  | .invokedynamic idx => writeU8 (writeU8 (writeU16 (writeU8 ba 0xba) idx) 0) 0

end Opcode

end Lean.Compiler.JVM
