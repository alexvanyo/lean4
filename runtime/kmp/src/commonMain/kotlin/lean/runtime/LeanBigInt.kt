/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

public expect class LeanBigInt

public expect fun createBigInt(str: String): LeanBigInt
public expect fun createBigInt(longVal: Long): LeanBigInt
public expect fun bigIntShiftRight(a: LeanBigInt, shift: Int): LeanBigInt
public expect fun bigIntShiftLeft(a: LeanBigInt, shift: Int): LeanBigInt
public expect fun bigIntAnd(a: LeanBigInt, b: LeanBigInt): LeanBigInt
public expect fun bigIntOr(a: LeanBigInt, b: LeanBigInt): LeanBigInt
public expect fun bigIntXor(a: LeanBigInt, b: LeanBigInt): LeanBigInt
public expect fun bigIntToLong(a: LeanBigInt): Long
public expect fun bigIntToInt(a: LeanBigInt): Int
public expect fun bigIntToDouble(a: LeanBigInt): Double
public expect fun bigIntCompare(a: LeanBigInt, b: LeanBigInt): Int
public expect fun bigIntIsZero(a: LeanBigInt): Boolean
public expect fun bigIntIsOne(a: LeanBigInt): Boolean
