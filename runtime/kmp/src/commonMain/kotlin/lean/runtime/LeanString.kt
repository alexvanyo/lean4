/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

import kotlin.jvm.JvmField
import kotlin.jvm.JvmStatic

/**
 * Representation of a Lean String.
 * Lean strings are UTF-8 byte sequences; positions and lengths in Lean are byte offsets.
 */
public final class LeanString(
    @JvmField public val bytes: ByteArray
) : LeanObject() {

    public val byteSize: Int get() = bytes.size

    public fun substringByByte(startByte: Int, endByte: Int): LeanString {
        val len = endByte - startByte
        val res = ByteArray(len)
        bytes.copyInto(res, 0, startByte, endByte)
        return LeanString(res)
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is LeanString) return false
        return bytes.contentEquals(other.bytes)
    }

    override fun hashCode(): Int = bytes.contentHashCode()

    override fun toString(): String = bytes.decodeToString()

    companion object {
        @JvmStatic
        public fun of(str: String): LeanString = LeanString(str.encodeToByteArray())

        @JvmStatic
        public fun empty(): LeanString = LeanString(ByteArray(0))

        @JvmStatic
        public fun concat(a: LeanString, b: LeanString): LeanString {
            val res = ByteArray(a.bytes.size + b.bytes.size)
            a.bytes.copyInto(res, 0, 0, a.bytes.size)
            b.bytes.copyInto(res, a.bytes.size, 0, b.bytes.size)
            return LeanString(res)
        }
    }
}
