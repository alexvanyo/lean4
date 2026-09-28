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
        if (smallVal >= 0L) {
            this.smallVal = smallVal
            this.bigVal = null
        } else {
            this.smallVal = -1L
            this.bigVal = bigIntFromULong(smallVal)
        }
    }

    public constructor(bigVal: LeanBigInt?) {
        if (bigVal == null || bigIntSignum(bigVal) <= 0) {
            this.smallVal = 0L
            this.bigVal = null
        } else if (bigIntCompare(bigVal, BIG_LONG_MAX) <= 0) {
            this.smallVal = bigIntToLong(bigVal)
            this.bigVal = null
        } else {
            this.smallVal = -1L
            this.bigVal = bigVal
        }
    }

    public fun toBigInteger(): LeanBigInt {
        val b = bigVal
        if (b != null) return b
        return createBigInt(smallVal)
    }

    public fun isZero(): Boolean = bigVal == null && smallVal == 0L
    public fun add(other: LeanNat): LeanNat = LeanNat.add(this, other)
    public fun sub(other: LeanNat): LeanNat = LeanNat.sub(this, other)
    public fun mul(other: LeanNat): LeanNat = LeanNat.mul(this, other)
    public fun div(other: LeanNat): LeanNat = LeanNat.div(this, other)
    public fun mod(other: LeanNat): LeanNat = LeanNat.mod(this, other)
    public fun pow(other: LeanNat): LeanNat = LeanNat.pow(this, other)
    public fun gcd(other: LeanNat): LeanNat = LeanNat.gcd(this, other)
    public fun le(other: LeanNat): Boolean = LeanNat.ble(this, other)
    public fun lt(other: LeanNat): Boolean = LeanNat.blt(this, other)
    public fun eq(other: LeanNat): Boolean = this == other

    public override val tag: Int
        get() {
            val b = bigVal
            if (b != null) return -1
            return if (smallVal in 0L..Int.MAX_VALUE.toLong()) smallVal.toInt() else -1
        }

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
        private val BIG_LONG_MAX: LeanBigInt = createBigInt(Long.MAX_VALUE)

        @JvmStatic
        public val ZERO: LeanNat = LeanNat(0L)
        @JvmStatic
        public val ONE: LeanNat = LeanNat(1L)

        @JvmStatic
        public fun ofLong(v: Long): LeanNat =
            if (v == 0L) ZERO else if (v == 1L) ONE else LeanNat(v)

        @JvmStatic
        public fun ofBigInteger(b: LeanBigInt?): LeanNat {
            if (b == null || bigIntIsZero(b) || bigIntSignum(b) < 0) return ZERO
            if (bigIntIsOne(b)) return ONE
            return LeanNat(b)
        }

        @JvmStatic
        public fun ofDecString(s: String): LeanNat {
            val l = s.toLongOrNull()
            if (l != null && l >= 0L) return ofLong(l)
            return LeanNat(createBigInt(s))
        }

        @JvmStatic
        public fun add(a: LeanNat, b: LeanNat): LeanNat {
            if (a.bigVal == null && b.bigVal == null) {
                val sum = a.smallVal + b.smallVal
                if (sum >= 0L) return ofLong(sum)
            }
            return ofBigInteger(bigIntAdd(a.toBigInteger(), b.toBigInteger()))
        }

        @JvmStatic
        public fun sub(a: LeanNat, b: LeanNat): LeanNat {
            if (a.bigVal == null && b.bigVal == null) {
                return if (a.smallVal <= b.smallVal) ZERO else ofLong(a.smallVal - b.smallVal)
            }
            val aBig = a.toBigInteger()
            val bBig = b.toBigInteger()
            if (bigIntCompare(aBig, bBig) <= 0) return ZERO
            return ofBigInteger(bigIntSub(aBig, bBig))
        }

        @JvmStatic
        public fun mul(a: LeanNat, b: LeanNat): LeanNat {
            if (a.bigVal == null && b.bigVal == null) {
                if (a.smallVal == 0L || b.smallVal == 0L) return ZERO
                if (a.smallVal == 1L) return b
                if (b.smallVal == 1L) return a
                if (a.smallVal <= Long.MAX_VALUE / b.smallVal) {
                    return ofLong(a.smallVal * b.smallVal)
                }
            }
            return ofBigInteger(bigIntMul(a.toBigInteger(), b.toBigInteger()))
        }

        @JvmStatic
        public fun div(a: LeanNat, b: LeanNat): LeanNat {
            if (b.bigVal == null && b.smallVal == 0L) return ZERO
            if (a.bigVal == null && b.bigVal == null) {
                return ofLong(a.smallVal / b.smallVal)
            }
            val bBig = b.toBigInteger()
            if (bigIntIsZero(bBig)) return ZERO
            return ofBigInteger(bigIntDiv(a.toBigInteger(), bBig))
        }

        @JvmStatic
        public fun mod(a: LeanNat, b: LeanNat): LeanNat {
            if (b.bigVal == null && b.smallVal == 0L) return ZERO
            if (a.bigVal == null && b.bigVal == null) {
                return ofLong(a.smallVal % b.smallVal)
            }
            val bBig = b.toBigInteger()
            if (bigIntIsZero(bBig)) return ZERO
            return ofBigInteger(bigIntMod(a.toBigInteger(), bBig))
        }

        @JvmStatic
        public fun ble(a: LeanNat, b: LeanNat): Boolean {
            val aBig = a.bigVal
            val bBig = b.bigVal
            if (aBig == null && bBig == null) return a.smallVal <= b.smallVal
            if (aBig == null) return true
            if (bBig == null) return false
            return bigIntCompare(aBig, bBig) <= 0
        }

        @JvmStatic
        public fun blt(a: LeanNat, b: LeanNat): Boolean {
            val aBig = a.bigVal
            val bBig = b.bigVal
            if (aBig == null && bBig == null) return a.smallVal < b.smallVal
            if (aBig == null) return true
            if (bBig == null) return false
            return bigIntCompare(aBig, bBig) < 0
        }

        @JvmStatic
        public fun pow(a: LeanNat, b: LeanNat): LeanNat {
            if (b.bigVal == null && b.smallVal == 0L) return ONE
            if (a.bigVal == null && a.smallVal == 0L) return ZERO
            if (a.bigVal == null && a.smallVal == 1L) return ONE
            if (b.bigVal != null) return ZERO
            if (b.smallVal > Int.MAX_VALUE) return ZERO
            return ofBigInteger(bigIntPow(a.toBigInteger(), b.smallVal.toInt()))
        }

        @JvmStatic
        public fun gcd(a: LeanNat, b: LeanNat): LeanNat {
            if (a.bigVal == null && b.bigVal == null) {
                var x = a.smallVal
                var y = b.smallVal
                while (y != 0L) {
                    val t = y
                    y = x % y
                    x = t
                }
                return ofLong(x)
            }
            return ofBigInteger(bigIntGcd(a.toBigInteger(), b.toBigInteger()))
        }
    }
}
