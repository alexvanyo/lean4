import Lean.Compiler.Kotlin

/-!
Tests the `_boxed` wrappers of functions that take arrays as well as scalars: the wrapper receives
every argument as `Any?` and must cast the arrays to the parameter types of the unboxed function.
-/

set_option linter.unusedVariables false

@[export lean_sum_from]
def sumFrom (xs : Array UInt32) (ys : Array UInt64) (i : Nat) (bias : UInt32) : UInt64 :=
  (xs[i]! + bias).toUInt64 + ys[i]!

@[export lean_count_above]
def countAbove (bs : ByteArray) (fs : FloatArray) (limit : UInt8) (scale : Float) : UInt32 :=
  (bs.toList.filter (· > limit)).length.toUInt32 + (fs.toList.filter (· > scale)).length.toUInt32
