/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

import kotlin.jvm.JvmField
import kotlin.jvm.JvmStatic

public expect fun formatDouble6(v: Double): String

/**
 * Representation of a Lean Float (64-bit IEEE 754 floating-point number).
 */
public final class LeanFloat(
    @JvmField public val value: Double
) : LeanObject() {
    public override val tag: Int get() = 0

    override fun toString(): String {
        return format(value)
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is LeanFloat) return false
        return value.toRawBits() == other.value.toRawBits()
    }

    override fun hashCode(): Int = value.hashCode()

    companion object {
        @JvmStatic public val ZERO: LeanFloat = LeanFloat(0.0)
        @JvmStatic public val ONE: LeanFloat = LeanFloat(1.0)
        @JvmStatic public val NAN: LeanFloat = LeanFloat(Double.NaN)
        @JvmStatic public val POS_INF: LeanFloat = LeanFloat(Double.POSITIVE_INFINITY)
        @JvmStatic public val NEG_INF: LeanFloat = LeanFloat(Double.NEGATIVE_INFINITY)

        @JvmStatic
        public fun ofDouble(v: Double): LeanFloat = LeanFloat(v)

        @JvmStatic
        public fun toDouble(obj: LeanObject?): Double {
            if (obj is LeanFloat) return obj.value
            if (obj is LeanNat) {
                return Double.fromBits(obj.smallVal.toLong())
            }
            if (obj is LeanCtor && obj.hasScalars()) {
                return Double.fromBits(obj.getScalar0())
            }
            return 0.0
        }

        @JvmStatic
        public fun format(v: Double): String {
            if (v.isNaN()) return "NaN"
            if (v == Double.POSITIVE_INFINITY) return "inf"
            if (v == Double.NEGATIVE_INFINITY) return "-inf"
            return formatDouble6(v)
        }
    }
}
