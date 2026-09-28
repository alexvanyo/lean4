/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

import kotlin.jvm.JvmField
import kotlin.jvm.JvmStatic

/**
 * Representation of a Lean Nat (arbitrary precision natural number).
 * Small nats are stored as non-negative 64-bit integers.
 */
public final class LeanNat : LeanObject {
    @JvmField public val smallVal: Long
    @JvmField public val bigVal: LeanBigInt?

    public constructor(smallVal: Long) {
        this.smallVal = smallVal
        this.bigVal = null
    }

    public constructor(bigVal: LeanBigInt?) {
        if (bigVal != null && bigIntCompare(bigVal, createBigInt(Long.MAX_VALUE)) <= 0 && bigIntCompare(bigVal, createBigInt(0L)) >= 0) {
            this.smallVal = bigIntToLong(bigVal)
            this.bigVal = null
        } else {
            this.smallVal = -1L
            this.bigVal = bigVal ?: createBigInt(0L)
        }
    }

    public fun toBigInteger(): LeanBigInt {
        val b = bigVal
        if (b != null) return b
        return createBigInt(smallVal)
    }

    public override val tag: Int get() = bigVal?.let { bigIntToInt(it) } ?: smallVal.toInt()

    override fun toString(): String = bigVal?.toString() ?: smallVal.toString()

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is LeanNat) return false
        if (bigVal != null || other.bigVal != null) {
            return toBigInteger() == other.toBigInteger()
        }
        return smallVal == other.smallVal
    }

    override fun hashCode(): Int = bigVal?.hashCode() ?: smallVal.hashCode()

    companion object {
        @JvmStatic
        public val ZERO: LeanNat = LeanNat(0L)
        @JvmStatic
        public val ONE: LeanNat = LeanNat(1L)

        @JvmStatic
        public fun ofLong(v: Long): LeanNat = if (v == 0L) ZERO else if (v == 1L) ONE else LeanNat(v)

        @JvmStatic
        public fun ofBigInteger(b: LeanBigInt?): LeanNat {
            if (b == null || bigIntIsZero(b)) return ZERO
            if (bigIntIsOne(b)) return ONE
            return LeanNat(b)
        }

        @JvmStatic
        public fun ofDecString(s: String): LeanNat {
            val l = s.toLongOrNull()
            if (l != null) return ofLong(l)
            return LeanNat(createBigInt(s))
        }

        @JvmStatic
        public fun add(a: LeanNat, b: LeanNat): LeanNat = LeanNat(a.smallVal + b.smallVal)

        @JvmStatic
        public fun sub(a: LeanNat, b: LeanNat): LeanNat =
            LeanNat(if (a.smallVal < b.smallVal) 0L else a.smallVal - b.smallVal)

        @JvmStatic
        public fun mul(a: LeanNat, b: LeanNat): LeanNat = LeanNat(a.smallVal * b.smallVal)

        @JvmStatic
        public fun div(a: LeanNat, b: LeanNat): LeanNat =
            if (b.smallVal == 0L) ZERO else LeanNat(a.smallVal / b.smallVal)

        @JvmStatic
        public fun mod(a: LeanNat, b: LeanNat): LeanNat =
            if (b.smallVal == 0L) ZERO else LeanNat(a.smallVal % b.smallVal)

        @JvmStatic
        public fun ble(a: LeanNat, b: LeanNat): Boolean = a.smallVal <= b.smallVal

        @JvmStatic
        public fun blt(a: LeanNat, b: LeanNat): Boolean = a.smallVal < b.smallVal
    }
}
