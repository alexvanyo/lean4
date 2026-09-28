/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

import kotlin.jvm.JvmField
import kotlin.jvm.JvmStatic

/**
 * Representation of a Lean inductive datatype constructor.
 * Contains:
 * - [tag]: Constructor index (cidx)
 * - [objs]: Object fields (pointers to other LeanObject instances)
 * - [scalars]: Packed 64-bit scalar fields (UInt8..UInt64, Float, USize)
 */
public final class LeanCtor(
    public override val tag: Int,
    @JvmField public val objs: Array<LeanObject?> = EMPTY_OBJS,
    @JvmField public val scalars: LongArray = EMPTY_SCALARS
) : LeanObject() {

    public fun getObj(index: Int): LeanObject? = objs[index]
    public fun setObj(index: Int, value: LeanObject?) { objs[index] = value }

    public fun getScalar(offset: Int): Long {
        val idx = if (offset >= objs.size) offset - objs.size else offset
        return if (idx in scalars.indices) scalars[idx] else 0L
    }

    public fun setScalar(offset: Int, value: Long) {
        val idx = if (offset >= objs.size) offset - objs.size else offset
        if (idx in scalars.indices) scalars[idx] = value
    }

    public fun getByteScalar(n: Int, byteOffset: Int, numBytes: Int): Long {
        val actualByteOffset = maxOf(0, n - objs.size) * 8 + byteOffset
        val wordIdx = actualByteOffset / 8
        if (wordIdx >= scalars.size) return 0L
        val shift = (actualByteOffset % 8) * 8
        val word = scalars[wordIdx]
        return when (numBytes) {
            1 -> (word ushr shift) and 0xFFL
            2 -> (word ushr shift) and 0xFFFFL
            4 -> (word ushr shift) and 0xFFFFFFFFL
            else -> word
        }
    }

    public fun setByteScalar(n: Int, byteOffset: Int, numBytes: Int, value: Long) {
        val actualByteOffset = maxOf(0, n - objs.size) * 8 + byteOffset
        val wordIdx = actualByteOffset / 8
        if (wordIdx >= scalars.size) return
        val shift = (actualByteOffset % 8) * 8
        if (numBytes >= 8) {
            scalars[wordIdx] = value
            return
        }
        val mask = when (numBytes) {
            1 -> 0xFFL shl shift
            2 -> 0xFFFFL shl shift
            4 -> 0xFFFFFFFFL shl shift
            else -> -1L
        }
        val current = scalars[wordIdx]
        val cleared = current and mask.inv()
        val maskedValue = when (numBytes) {
            1 -> value and 0xFFL
            2 -> value and 0xFFFFL
            4 -> value and 0xFFFFFFFFL
            else -> value
        }
        scalars[wordIdx] = cleared or (maskedValue shl shift)
    }

    override fun toString(): String {
        return "LeanCtor(tag=$tag, objs=${objs.contentToString()}, scalars=${scalars.contentToString()})"
    }

    companion object {
        private val EMPTY_SCALARS = LongArray(0)
        private val EMPTY_OBJS = emptyArray<LeanObject?>()

        @JvmStatic
        public fun alloc(tag: Int, numObjs: Int, numScalars: Int): LeanCtor {
            val objs = if (numObjs == 0) EMPTY_OBJS else arrayOfNulls(numObjs)
            val scalars = if (numScalars == 0) EMPTY_SCALARS else LongArray(numScalars)
            return LeanCtor(tag, objs, scalars)
        }
    }
}
