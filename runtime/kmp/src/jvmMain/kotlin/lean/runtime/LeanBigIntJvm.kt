/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

import java.math.BigInteger

public actual typealias LeanBigInt = BigInteger

public actual fun createBigInt(str: String): LeanBigInt = BigInteger(str)
public actual fun createBigInt(longVal: Long): LeanBigInt = BigInteger.valueOf(longVal)
public actual fun bigIntShiftRight(a: LeanBigInt, shift: Int): LeanBigInt = a.shiftRight(shift)
public actual fun bigIntShiftLeft(a: LeanBigInt, shift: Int): LeanBigInt = a.shiftLeft(shift)
public actual fun bigIntAnd(a: LeanBigInt, b: LeanBigInt): LeanBigInt = a.and(b)
public actual fun bigIntOr(a: LeanBigInt, b: LeanBigInt): LeanBigInt = a.or(b)
public actual fun bigIntXor(a: LeanBigInt, b: LeanBigInt): LeanBigInt = a.xor(b)
public actual fun bigIntToLong(a: LeanBigInt): Long = a.toLong()
public actual fun bigIntToInt(a: LeanBigInt): Int = a.toInt()
public actual fun bigIntCompare(a: LeanBigInt, b: LeanBigInt): Int = a.compareTo(b)
public actual fun bigIntIsZero(a: LeanBigInt): Boolean = a == BigInteger.ZERO
public actual fun bigIntIsOne(a: LeanBigInt): Boolean = a == BigInteger.ONE
