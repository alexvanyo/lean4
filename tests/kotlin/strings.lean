import Lean.Compiler.Kotlin

/-!
Tests String type mapping, literal escaping, concatenation, length, UTF-8 byte size,
character push, and comparisons in the Kotlin backend.
-/

def testStringEscape : String :=
  "line1\nline2\t\"quoted\" $dollar \\backslash"

def testStringConcat (a b : String) : String :=
  a ++ " - " ++ b

def testStringMetrics (s : String) : Nat :=
  if s.isEmpty then 0 else s.length + s.utf8ByteSize

def testStringCompare (a b : String) : Bool :=
  a == b || a < b
