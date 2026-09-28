/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

import kotlin.jvm.JvmField
import kotlin.jvm.JvmStatic

/**
 * Representation of a Lean ByteArray.
 */
public final class LeanByteArray(
    @JvmField public val data: ByteArray
) : LeanObject() {

    public fun size(): Int = data.size

    public fun get(index: Int): Byte = data[index]
    public fun getUByte(index: Int): UByte = data[index].toUByte()

    public fun push(b: Byte): LeanByteArray {
        val copy = data.copyOf(data.size + 1)
        copy[data.size] = b
        return LeanByteArray(copy)
    }

    public fun push(b: UByte): LeanByteArray = push(b.toByte())

    public fun set(index: Int, b: Byte): LeanByteArray {
        val copy = data.copyOf()
        copy[index] = b
        return LeanByteArray(copy)
    }

    public fun set(index: Int, b: UByte): LeanByteArray = set(index, b.toByte())

    companion object {
        private val EMPTY_BYTES = ByteArray(0)

        @JvmField
        public val EMPTY: LeanByteArray = LeanByteArray(EMPTY_BYTES)

        @JvmStatic
        public fun empty(): LeanByteArray = EMPTY

        @JvmStatic
        public fun of(bytes: ByteArray): LeanByteArray = LeanByteArray(bytes)
    }
}
