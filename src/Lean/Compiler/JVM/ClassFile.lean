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
public import Init.Data.String.Basic
public import Init.Data.Hashable
public import Lean.Compiler.JVM.Opcode
public import Std.Data.HashMap.Basic

public section

namespace Lean.Compiler.JVM

/--
Constant Pool Entry for JVM Classfiles.
-/
inductive CPEntry where
  | utf8 (s : String)
  | integer (v : UInt32)
  | long (v : UInt64)
  | classRef (nameIdx : UInt16)
  | stringRef (utf8Idx : UInt16)
  | fieldRef (classIdx : UInt16) (nameAndTypeIdx : UInt16)
  | methodRef (classIdx : UInt16) (nameAndTypeIdx : UInt16)
  | nameAndType (nameIdx : UInt16) (descIdx : UInt16)
  deriving Inhabited

structure ConstantPool where
  entries         : Array CPEntry := #[]
  utf8Map         : Std.HashMap String UInt16 := {}
  classMap        : Std.HashMap UInt16 UInt16 := {}
  stringMap       : Std.HashMap UInt16 UInt16 := {}
  integerMap      : Std.HashMap UInt32 UInt16 := {}
  nameAndTypeMap  : Std.HashMap (UInt16 × UInt16) UInt16 := {}
  methodRefMap    : Std.HashMap (UInt16 × UInt16) UInt16 := {}
  fieldRefMap     : Std.HashMap (UInt16 × UInt16) UInt16 := {}
  deriving Inhabited

namespace ConstantPool

def empty : ConstantPool := { entries := #[] }

def size (cp : ConstantPool) : Nat := cp.entries.size

/--
Adds an entry to the constant pool, returning its 1-based index and updated pool.
Note: In JVM class files, indices are 1-based.
Long entries occupy two slots (per JVM spec).
-/
def addEntry (cp : ConstantPool) (entry : CPEntry) : UInt16 × ConstantPool :=
  let idx := (cp.entries.size + 1).toUInt16
  match entry with
  | .long _ =>
    -- Long takes two pool entries (second is unused dummy)
    let newEntries := cp.entries.push entry |>.push (.utf8 "")
    (idx, { cp with entries := newEntries })
  | _ =>
    (idx, { cp with entries := cp.entries.push entry })

def addUtf8 (cp : ConstantPool) (s : String) : UInt16 × ConstantPool :=
  match cp.utf8Map[s]? with
  | some idx => (idx, cp)
  | none =>
    let (idx, cp') := cp.addEntry (.utf8 s)
    (idx, { cp' with utf8Map := cp'.utf8Map.insert s idx })

def addClass (cp : ConstantPool) (className : String) : UInt16 × ConstantPool :=
  let (nameIdx, cp) := cp.addUtf8 className
  match cp.classMap[nameIdx]? with
  | some idx => (idx, cp)
  | none =>
    let (idx, cp') := cp.addEntry (.classRef nameIdx)
    (idx, { cp' with classMap := cp'.classMap.insert nameIdx idx })

def addString (cp : ConstantPool) (str : String) : UInt16 × ConstantPool :=
  let (strIdx, cp) := cp.addUtf8 str
  match cp.stringMap[strIdx]? with
  | some idx => (idx, cp)
  | none =>
    let (idx, cp') := cp.addEntry (.stringRef strIdx)
    (idx, { cp' with stringMap := cp'.stringMap.insert strIdx idx })

def addInteger (cp : ConstantPool) (v : UInt32) : UInt16 × ConstantPool :=
  match cp.integerMap[v]? with
  | some idx => (idx, cp)
  | none =>
    let (idx, cp') := cp.addEntry (.integer v)
    (idx, { cp' with integerMap := cp'.integerMap.insert v idx })

def addNameAndType (cp : ConstantPool) (name : String) (desc : String) : UInt16 × ConstantPool :=
  let (nameIdx, cp) := cp.addUtf8 name
  let (descIdx, cp) := cp.addUtf8 desc
  let key := (nameIdx, descIdx)
  match cp.nameAndTypeMap[key]? with
  | some idx => (idx, cp)
  | none =>
    let (idx, cp') := cp.addEntry (.nameAndType nameIdx descIdx)
    (idx, { cp' with nameAndTypeMap := cp'.nameAndTypeMap.insert key idx })

def addMethodRef (cp : ConstantPool) (className : String) (methodName : String) (desc : String) : UInt16 × ConstantPool :=
  let (classIdx, cp) := cp.addClass className
  let (ntIdx, cp) := cp.addNameAndType methodName desc
  let key := (classIdx, ntIdx)
  match cp.methodRefMap[key]? with
  | some idx => (idx, cp)
  | none =>
    let (idx, cp') := cp.addEntry (.methodRef classIdx ntIdx)
    (idx, { cp' with methodRefMap := cp'.methodRefMap.insert key idx })

def addFieldRef (cp : ConstantPool) (className : String) (fieldName : String) (desc : String) : UInt16 × ConstantPool :=
  let (classIdx, cp) := cp.addClass className
  let (ntIdx, cp) := cp.addNameAndType fieldName desc
  let key := (classIdx, ntIdx)
  match cp.fieldRefMap[key]? with
  | some idx => (idx, cp)
  | none =>
    let (idx, cp') := cp.addEntry (.fieldRef classIdx ntIdx)
    (idx, { cp' with fieldRefMap := cp'.fieldRefMap.insert key idx })

end ConstantPool

structure MethodDef where
  accessFlags : UInt16
  name        : String
  descriptor  : String
  maxStack    : UInt16
  maxLocals   : UInt16
  bytecodes   : ByteArray
  stackMap?   : Option ByteArray := none
  deriving Inhabited

structure FieldDef where
  accessFlags : UInt16
  name        : String
  descriptor  : String
  deriving Inhabited

structure ClassFile where
  className  : String
  superClass : String := "java/lang/Object"
  cp         : ConstantPool := ConstantPool.empty
  fields     : Array FieldDef := #[]
  methods    : Array MethodDef := #[]
  deriving Inhabited

namespace ClassFile

def ACC_PUBLIC       : UInt16 := 0x0001
def ACC_PRIVATE      : UInt16 := 0x0002
def ACC_STATIC       : UInt16 := 0x0008
def ACC_FINAL        : UInt16 := 0x0010
def ACC_SUPER        : UInt16 := 0x0020

def addMethodRef (cf : ClassFile) (className : String) (methodName : String) (desc : String) : UInt16 × ClassFile :=
  let (idx, cp) := cf.cp.addMethodRef className methodName desc
  (idx, { cf with cp := cp })

def addFieldRef (cf : ClassFile) (className : String) (fieldName : String) (desc : String) : UInt16 × ClassFile :=
  let (idx, cp) := cf.cp.addFieldRef className fieldName desc
  (idx, { cf with cp := cp })

def addClass (cf : ClassFile) (className : String) : UInt16 × ClassFile :=
  let (idx, cp) := cf.cp.addClass className
  (idx, { cf with cp := cp })

def addString (cf : ClassFile) (str : String) : UInt16 × ClassFile :=
  let (idx, cp) := cf.cp.addString str
  (idx, { cf with cp := cp })

def addInteger (cf : ClassFile) (v : UInt32) : UInt16 × ClassFile :=
  let (idx, cp) := cf.cp.addInteger v
  (idx, { cf with cp := cp })

open Opcode (writeU8 writeU16 writeU32)

/--
Encodes the constant pool into a ByteArray.
-/
def encodeConstantPool (ba : ByteArray) (cp : ConstantPool) : ByteArray :=
  Id.run do
    let mut ba := ba
    for entry in cp.entries do
      match entry with
      | .utf8 s =>
        let utf8Bytes := s.toUTF8
        ba := writeU8 ba 1 -- Tag 1: CONSTANT_Utf8
        ba := writeU16 ba utf8Bytes.size.toUInt16
        ba := ba ++ utf8Bytes
      | .integer v =>
        ba := writeU8 ba 3 -- Tag 3: CONSTANT_Integer
        ba := writeU32 ba v
      | .long v =>
        ba := writeU8 ba 5 -- Tag 5: CONSTANT_Long
        ba := writeU32 ba (v >>> 32).toUInt32
        ba := writeU32 ba v.toUInt32
      | .classRef nameIdx =>
        ba := writeU8 ba 7 -- Tag 7: CONSTANT_Class
        ba := writeU16 ba nameIdx
      | .stringRef utf8Idx =>
        ba := writeU8 ba 8 -- Tag 8: CONSTANT_String
        ba := writeU16 ba utf8Idx
      | .fieldRef classIdx ntIdx =>
        ba := writeU8 ba 9 -- Tag 9: CONSTANT_Fieldref
        ba := writeU16 ba classIdx
        ba := writeU16 ba ntIdx
      | .methodRef classIdx ntIdx =>
        ba := writeU8 ba 10 -- Tag 10: CONSTANT_Methodref
        ba := writeU16 ba classIdx
        ba := writeU16 ba ntIdx
      | .nameAndType nameIdx descIdx =>
        ba := writeU8 ba 12 -- Tag 12: CONSTANT_NameAndType
        ba := writeU16 ba nameIdx
        ba := writeU16 ba descIdx
    return ba

/--
Serializes a ClassFile into standard JVM .class binary bytes.
Targeting Java 6+ (major version 50).
-/
def toByteArray (cf : ClassFile) : ByteArray :=
  Id.run do
    let mut cp := cf.cp

    -- Register this_class and super_class
    let (thisClassIdx, cp') := cp.addClass cf.className
    let (superClassIdx, cp') := cp'.addClass cf.superClass
    cp := cp'

    -- Register attribute names
    let (codeAttrIdx, cp') := cp.addUtf8 "Code"
    let (stackMapAttrIdx, cp') := cp'.addUtf8 "StackMapTable"
    cp := cp'

    -- Register fields
    let mut fieldData := #[]
    for f in cf.fields do
      let (nameIdx, cp') := cp.addUtf8 f.name
      let (descIdx, cp') := cp'.addUtf8 f.descriptor
      cp := cp'
      fieldData := fieldData.push (f.accessFlags, nameIdx, descIdx)

    -- Register methods
    let mut methodData := #[]
    for m in cf.methods do
      let (nameIdx, cp') := cp.addUtf8 m.name
      let (descIdx, cp') := cp'.addUtf8 m.descriptor
      cp := cp'
      methodData := methodData.push (m.accessFlags, nameIdx, descIdx, m.maxStack, m.maxLocals, m.bytecodes, m.stackMap?)

    -- Header: Magic (0xCAFEBABE), Minor version (0), Major version (50 = Java 6)
    let mut ba := ByteArray.empty
    ba := writeU32 ba 0xCAFEBABE
    ba := writeU16 ba 0 -- minor version
    ba := writeU16 ba 50 -- major version (Java 6: universally supported, verifier uses type inference without requiring StackMapTable)

    -- Constant Pool Count (cp.size + 1)
    ba := writeU16 ba (cp.size + 1).toUInt16
    ba := encodeConstantPool ba cp

    -- Class Access Flags
    ba := writeU16 ba (ACC_PUBLIC ||| ACC_SUPER)
    ba := writeU16 ba thisClassIdx
    ba := writeU16 ba superClassIdx

    -- Interfaces count = 0
    ba := writeU16 ba 0

    -- Fields count
    ba := writeU16 ba fieldData.size.toUInt16
    for (flags, nameIdx, descIdx) in fieldData do
      ba := writeU16 ba flags
      ba := writeU16 ba nameIdx
      ba := writeU16 ba descIdx
      ba := writeU16 ba 0 -- attributes_count

    -- Methods count
    ba := writeU16 ba methodData.size.toUInt16
    for (flags, nameIdx, descIdx, maxStack, maxLocals, code, stackMap?) in methodData do
      ba := writeU16 ba flags
      ba := writeU16 ba nameIdx
      ba := writeU16 ba descIdx
      ba := writeU16 ba 1 -- attributes_count = 1 (Code attribute)

      -- Code attribute
      ba := writeU16 ba codeAttrIdx
      match stackMap? with
      | some sm =>
        let attrLen := 2 + 2 + 4 + code.size.toUInt32 + 2 + 2 + (2 + 4 + sm.size.toUInt32)
        ba := writeU32 ba attrLen
        ba := writeU16 ba maxStack
        ba := writeU16 ba maxLocals
        ba := writeU32 ba code.size.toUInt32
        ba := ba ++ code
        ba := writeU16 ba 0 -- exception_table_length
        ba := writeU16 ba 1 -- code attributes_count = 1 (StackMapTable)
        ba := writeU16 ba stackMapAttrIdx
        ba := writeU32 ba sm.size.toUInt32
        ba := ba ++ sm
      | none =>
        let attrLen := 2 + 2 + 4 + code.size.toUInt32 + 2 + 2
        ba := writeU32 ba attrLen
        ba := writeU16 ba maxStack
        ba := writeU16 ba maxLocals
        ba := writeU32 ba code.size.toUInt32
        ba := ba ++ code
        ba := writeU16 ba 0 -- exception_table_length
        ba := writeU16 ba 0 -- code attributes_count = 0

    -- Class attributes_count = 0
    ba := writeU16 ba 0

    return ba

end ClassFile

end Lean.Compiler.JVM
