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
import Init.While
import Init.Data.Array.QSort.Basic

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

inductive VerificationType where
  | top
  | integer
  | float
  | double
  | long
  | null
  | uninitializedThis
  | object (classIdx : UInt16)
  | uninitialized (offset : UInt16)
  deriving Inhabited, BEq

def encodeVerificationType (ba : ByteArray) (vt : VerificationType) : ByteArray :=
  match vt with
  | .top => writeU8 ba 0
  | .integer => writeU8 ba 1
  | .float => writeU8 ba 2
  | .double => writeU8 ba 3
  | .long => writeU8 ba 4
  | .null => writeU8 ba 5
  | .uninitializedThis => writeU8 ba 6
  | .object classIdx => writeU16 (writeU8 ba 7) classIdx
  | .uninitialized offset => writeU16 (writeU8 ba 8) offset

def setLocal (locals : Array VerificationType) (slot : Nat) (vt : VerificationType) : Array VerificationType :=
  Id.run do
    let mut arr := locals
    while arr.size <= slot do
      arr := arr.push .top
    arr := arr.set! slot vt
    return arr

partial def parseParamTypes (desc : String) (cp : ConstantPool) : Array VerificationType × ConstantPool :=
  Id.run do
    let mut cp := cp
    let mut types : Array VerificationType := #[]
    let mut i := 0
    let chars := desc.toList.toArray
    while i < chars.size && chars[i]! != '(' do
      i := i + 1
    if i < chars.size then
      i := i + 1
    while i < chars.size && chars[i]! != ')' do
      let c := chars[i]!
      match c with
      | 'I' | 'B' | 'S' | 'C' | 'Z' =>
        types := types.push .integer
        i := i + 1
      | 'J' =>
        types := types.push .long
        i := i + 1
      | 'F' =>
        types := types.push .float
        i := i + 1
      | 'D' =>
        types := types.push .double
        i := i + 1
      | 'L' =>
        i := i + 1
        let mut className := ""
        while i < chars.size && chars[i]! != ';' do
          className := className.push chars[i]!
          i := i + 1
        if i < chars.size then i := i + 1
        let (classIdx, cp') := cp.addClass className
        cp := cp'
        types := types.push (.object classIdx)
      | '[' =>
        let mut arrType := "["
        i := i + 1
        while i < chars.size && chars[i]! == '[' do
          arrType := arrType.push '['
          i := i + 1
        if i < chars.size && chars[i]! == 'L' then
          while i < chars.size && chars[i]! != ';' do
            arrType := arrType.push chars[i]!
            i := i + 1
          if i < chars.size then
            arrType := arrType.push ';'
            i := i + 1
        else if i < chars.size then
          arrType := arrType.push chars[i]!
          i := i + 1
        let (arrIdx, cp') := cp.addClass arrType
        cp := cp'
        types := types.push (.object arrIdx)
      | _ =>
        i := i + 1
    return (types, cp)

def trimTop (locals : Array VerificationType) : Array VerificationType :=
  Id.run do
    let mut arr := locals
    while arr.size > 0 && arr.back! == .top do
      arr := arr.pop
    return arr

def computeStackMapTable (m : MethodDef) (thisClassIdx : UInt16) (cp : ConstantPool) : Option ByteArray × ConstantPool :=
  Id.run do
    let code := m.bytecodes
    if code.size == 0 then return (none, cp)

    let (leanObjIdx, cp') := cp.addClass "lean/runtime/LeanObject"
    let mut cp := cp'
    let (paramTypes, cp') := parseParamTypes m.descriptor cp
    cp := cp'

    let mut initialLocals : Array VerificationType := #[]
    if (m.accessFlags &&& ACC_STATIC) == 0 then
      initialLocals := initialLocals.push (.object thisClassIdx)
    for pt in paramTypes do
      initialLocals := initialLocals.push pt
      if pt == .long || pt == .double then
        initialLocals := initialLocals.push .top

    let mut targets : Array Nat := #[]
    let mut targetLocalsMap : Std.HashMap Nat (Array VerificationType) := {}
    let mut currentLocals := initialLocals
    let mut pc : Nat := 0

    while pc < code.size do
      if let some locs := targetLocalsMap[pc]? then
        currentLocals := locs

      let op := code.get! pc
      let nextPc : Nat := match op with
        | 0x00 | 0x01 | 0x02 | 0x03 | 0x04 | 0x05 | 0x06 | 0x07 | 0x08 | 0x09 | 0x0a => pc + 1
        | 0x10 => pc + 2
        | 0x11 => pc + 3
        | 0x12 => pc + 2
        | 0x13 | 0x14 => pc + 3
        | 0x15 | 0x16 | 0x17 | 0x18 | 0x19 => pc + 2
        | 0x1a | 0x1b | 0x1c | 0x1d | 0x1e | 0x1f | 0x20 | 0x21
        | 0x22 | 0x23 | 0x24 | 0x25 | 0x26 | 0x27 | 0x28 | 0x29
        | 0x2a | 0x2b | 0x2c | 0x2d | 0x2e | 0x2f | 0x30 | 0x31
        | 0x32 | 0x33 | 0x34 | 0x35 => pc + 1
        | 0x36 | 0x37 | 0x38 | 0x39 | 0x3a => pc + 2
        | 0x3b | 0x3c | 0x3d | 0x3e | 0x3f | 0x40 | 0x41 | 0x42
        | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 | 0x48 | 0x49 | 0x4a
        | 0x4b | 0x4c | 0x4d | 0x4e | 0x4f | 0x50 | 0x51 | 0x52
        | 0x53 | 0x54 | 0x55 | 0x56 => pc + 1
        | 0x57 | 0x58 | 0x59 | 0x5a | 0x5b | 0x5c | 0x5d | 0x5e | 0x5f => pc + 1
        | 0x60 | 0x61 | 0x62 | 0x63 | 0x64 | 0x65 | 0x66 | 0x67
        | 0x68 | 0x69 | 0x6a | 0x6b | 0x6c | 0x6d | 0x6e | 0x6f
        | 0x70 | 0x71 | 0x72 | 0x73 | 0x74 | 0x75 | 0x76 | 0x77
        | 0x78 | 0x79 | 0x7a | 0x7b | 0x7c | 0x7d | 0x7e | 0x7f
        | 0x80 | 0x81 | 0x82 | 0x83 => pc + 1
        | 0x84 => pc + 3
        | 0x85 | 0x86 | 0x87 | 0x88 | 0x89 | 0x8a | 0x8b | 0x8c
        | 0x8d | 0x8e | 0x8f | 0x90 | 0x91 | 0x92 | 0x93 => pc + 1
        | 0x94 | 0x95 | 0x96 | 0x97 | 0x98 => pc + 1
        | 0x99 | 0x9a | 0x9b | 0x9c | 0x9d | 0x9e
        | 0x9f | 0xa0 | 0xa1 | 0xa2 | 0xa3 | 0xa4
        | 0xa5 | 0xa6 | 0xa7 | 0xa8 => pc + 3
        | 0xa9 => pc + 2
        | 0xaa =>
          let pad := (4 - ((pc + 1) % 4)) % 4
          let low := ((code.get! (pc + 1 + pad + 4)).toUInt32 <<< 24) |||
                     ((code.get! (pc + 1 + pad + 5)).toUInt32 <<< 16) |||
                     ((code.get! (pc + 1 + pad + 6)).toUInt32 <<< 8) |||
                     (code.get! (pc + 1 + pad + 7)).toUInt32
          let high := ((code.get! (pc + 1 + pad + 8)).toUInt32 <<< 24) |||
                      ((code.get! (pc + 1 + pad + 9)).toUInt32 <<< 16) |||
                      ((code.get! (pc + 1 + pad + 10)).toUInt32 <<< 8) |||
                      (code.get! (pc + 1 + pad + 11)).toUInt32
          let count := high.toNat - low.toNat + 1
          pc + 1 + pad + 12 + count * 4
        | 0xac | 0xad | 0xae | 0xaf | 0xb0 | 0xb1 => pc + 1
        | 0xb2 | 0xb3 | 0xb4 | 0xb5 => pc + 3
        | 0xb6 | 0xb7 | 0xb8 => pc + 3
        | 0xb9 | 0xba => pc + 5
        | 0xbb => pc + 3
        | 0xbc => pc + 2
        | 0xbd => pc + 3
        | 0xbe | 0xbf => pc + 1
        | 0xc0 | 0xc1 => pc + 3
        | 0xc2 | 0xc3 => pc + 1
        | 0xc5 => pc + 4
        | 0xc6 | 0xc7 => pc + 3
        | 0xc8 => pc + 5
        | _ => pc + 1

      match op with
      | 0x36 =>
        if pc + 1 < code.size then
          let slot := (code.get! (pc + 1)).toNat
          currentLocals := setLocal currentLocals slot .integer
      | 0x37 =>
        if pc + 1 < code.size then
          let slot := (code.get! (pc + 1)).toNat
          currentLocals := setLocal currentLocals slot .long
          currentLocals := setLocal currentLocals (slot + 1) .top
      | 0x38 =>
        if pc + 1 < code.size then
          let slot := (code.get! (pc + 1)).toNat
          currentLocals := setLocal currentLocals slot .float
      | 0x39 =>
        if pc + 1 < code.size then
          let slot := (code.get! (pc + 1)).toNat
          currentLocals := setLocal currentLocals slot .double
          currentLocals := setLocal currentLocals (slot + 1) .top
      | 0x3a =>
        if pc + 1 < code.size then
          let slot := (code.get! (pc + 1)).toNat
          currentLocals := setLocal currentLocals slot (.object leanObjIdx)
      | 0x3b => currentLocals := setLocal currentLocals 0 .integer
      | 0x3c => currentLocals := setLocal currentLocals 1 .integer
      | 0x3d => currentLocals := setLocal currentLocals 2 .integer
      | 0x3e => currentLocals := setLocal currentLocals 3 .integer
      | 0x3f =>
        currentLocals := setLocal currentLocals 0 .long
        currentLocals := setLocal currentLocals 1 .top
      | 0x40 =>
        currentLocals := setLocal currentLocals 1 .long
        currentLocals := setLocal currentLocals 2 .top
      | 0x41 =>
        currentLocals := setLocal currentLocals 2 .long
        currentLocals := setLocal currentLocals 3 .top
      | 0x42 =>
        currentLocals := setLocal currentLocals 3 .long
        currentLocals := setLocal currentLocals 4 .top
      | 0x4b => currentLocals := setLocal currentLocals 0 (.object leanObjIdx)
      | 0x4c => currentLocals := setLocal currentLocals 1 (.object leanObjIdx)
      | 0x4d => currentLocals := setLocal currentLocals 2 (.object leanObjIdx)
      | 0x4e => currentLocals := setLocal currentLocals 3 (.object leanObjIdx)
      | _ => ()

      if (op >= 0x99 && op <= 0xa8) || op == 0xc6 || op == 0xc7 then
        if pc + 2 < code.size then
          let hi := (code.get! (pc + 1)).toUInt16
          let lo := (code.get! (pc + 2)).toUInt16
          let u16Val := (hi <<< 8) ||| lo
          let target := if u16Val >= 0x8000 then
            (pc : Int) - (0x10000 - u16Val.toNat : Int)
          else
            (pc : Int) + (u16Val.toNat : Int)
          if target >= 0 then
            let tNat := target.toNat
            if !targets.contains tNat then
              targets := targets.push tNat
            if !targetLocalsMap.contains tNat then
              targetLocalsMap := targetLocalsMap.insert tNat (trimTop currentLocals)

      if (op >= 0xac && op <= 0xb1) || op == 0xa7 then
        match Nat.decLt nextPc code.size with
        | .isTrue _ =>
          if !targets.contains nextPc then
            targets := targets.push nextPc
          if !targetLocalsMap.contains nextPc then
            targetLocalsMap := targetLocalsMap.insert nextPc (trimTop currentLocals)
        | .isFalse _ => ()
        if let some locs := targetLocalsMap[nextPc]? then
          currentLocals := locs
        else
          currentLocals := initialLocals

      pc := nextPc

    if targets.isEmpty then
      return (none, cp)

    let sortedTargets := targets.qsort (· < ·)
    let mut sm := ByteArray.empty
    sm := writeU16 sm sortedTargets.size.toUInt16

    let mut prevTarget : Int := -1
    let mut prevLocals := initialLocals

    for t in sortedTargets do
      let delta : Nat := if prevTarget < 0 then t else t - prevTarget.toNat - 1
      let localsAtT := match targetLocalsMap[t]? with
        | some locs => locs
        | none => initialLocals

      if localsAtT == prevLocals then
        if delta < 64 then
          sm := writeU8 sm delta.toUInt8
        else
          sm := writeU8 sm 251
          sm := writeU16 sm delta.toUInt16
      else
        sm := writeU8 sm 255
        sm := writeU16 sm delta.toUInt16
        sm := writeU16 sm localsAtT.size.toUInt16
        for vt in localsAtT do
          sm := encodeVerificationType sm vt
        sm := writeU16 sm 0

      prevTarget := t
      prevLocals := localsAtT

    return (some sm, cp)

/--
Serializes a ClassFile into standard JVM .class binary bytes.
Targeting Java 17+ (major version 61).
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
      let (sm?, cp') := match m.stackMap? with
        | some sm => (some sm, cp)
        | none => computeStackMapTable m thisClassIdx cp
      cp := cp'
      methodData := methodData.push (m.accessFlags, nameIdx, descIdx, m.maxStack, m.maxLocals, m.bytecodes, sm?)

    -- Header: Magic (0xCAFEBABE), Minor version (0), Major version (61 = Java 17)
    let mut ba := ByteArray.empty
    ba := writeU32 ba 0xCAFEBABE
    ba := writeU16 ba 0 -- minor version
    ba := writeU16 ba 61 -- major version (Java 17)

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
