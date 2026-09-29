/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

import kotlin.jvm.JvmField
import kotlin.jvm.JvmOverloads
import kotlin.jvm.JvmStatic

/**
 * Representation of a Lean inductive datatype constructor.
 * Contains:
 * - [tag]: Constructor index (cidx)
 * - [objs]: Object fields (pointers to other LeanObject instances)
 * - [scalars]: Packed 64-bit scalar fields (UInt8..UInt64, Float, USize)
 */
public open class LeanCtor(
    public override val tag: Int,
    @JvmField public val objs: Array<LeanObject?> = EMPTY_OBJS,
    @JvmField public val scalars: LongArray = EMPTY_SCALARS
) : LeanObject() {

    public constructor(tag: Int) : this(tag, EMPTY_OBJS, EMPTY_SCALARS)
    public constructor(tag: Int, objs: Array<LeanObject?>) : this(tag, objs, EMPTY_SCALARS)

    public open val numObjs: Int get() = objs.size
    public open val numScalars: Int get() = scalars.size
    public open fun hasScalars(): Boolean = scalars.isNotEmpty()

    public open fun getObj(index: Int): LeanObject? = if (index in objs.indices) objs[index] else null
    public open fun setObj(index: Int, value: LeanObject?) { if (index in objs.indices) objs[index] = value }

    public open fun getObj0(): LeanObject? = getObj(0)
    public open fun setObj0(value: LeanObject?) { setObj(0, value) }

    public open fun getObj1(): LeanObject? = getObj(1)
    public open fun setObj1(value: LeanObject?) { setObj(1, value) }

    public open fun getScalar(offset: Int): Long {
        val idx = if (offset >= objs.size) offset - objs.size else offset
        return if (idx in scalars.indices) scalars[idx] else 0L
    }

    public open fun setScalar(offset: Int, value: Long) {
        val idx = if (offset >= objs.size) offset - objs.size else offset
        if (idx in scalars.indices) scalars[idx] = value
    }

    public open fun getScalar0(): Long = getScalar(0)
    public open fun setScalar0(value: Long) { setScalar(0, value) }

    public open fun getByteScalar(n: Int, byteOffset: Int, numBytes: Int): Long {
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

    public open fun setByteScalar(n: Int, byteOffset: Int, numBytes: Int, value: Long) {
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

    public open fun getByteScalar0(byteOffset: Int, numBytes: Int): Long =
        getByteScalar(objs.size, byteOffset, numBytes)

    public open fun setByteScalar0(byteOffset: Int, numBytes: Int, value: Long) {
        setByteScalar(objs.size, byteOffset, numBytes, value)
    }

    override fun toString(): String {
        return "LeanCtor(tag=$tag, objs=${objs.contentToString()}, scalars=${scalars.contentToString()})"
    }

    companion object {
        @JvmField public val EMPTY_SCALARS: LongArray = LongArray(0)
        @JvmField public val EMPTY_OBJS: Array<LeanObject?> = emptyArray()

        private val CACHE0: Array<LeanCtor0> = Array(16) { LeanCtor0(it) }

        @JvmStatic
        public fun alloc0(tag: Int): LeanCtor =
            if (tag in 0..15) CACHE0[tag] else LeanCtor0(tag)

        @JvmStatic
        public fun alloc1(tag: Int, o0: LeanObject?): LeanCtor =
            LeanCtor1(tag, o0)

        @JvmStatic
        public fun alloc2(tag: Int, o0: LeanObject?, o1: LeanObject?): LeanCtor =
            LeanCtor2(tag, o0, o1)

        @JvmStatic
        @JvmOverloads
        public fun allocScalar1(tag: Int, s0: Long = 0L): LeanCtor =
            LeanCtorScalar1(tag, s0)

        @JvmStatic
        public fun alloc(tag: Int, numObjs: Int, numScalars: Int): LeanCtor {
            return when {
                numObjs == 0 && numScalars == 0 -> alloc0(tag)
                numObjs == 1 && numScalars == 0 -> LeanCtor1(tag, null)
                numObjs == 2 && numScalars == 0 -> LeanCtor2(tag, null, null)
                numObjs == 0 && numScalars == 1 -> LeanCtorScalar1(tag, 0L)
                else -> {
                    val objs = if (numObjs == 0) EMPTY_OBJS else arrayOfNulls(numObjs)
                    val scalars = if (numScalars == 0) EMPTY_SCALARS else LongArray(numScalars)
                    LeanCtor(tag, objs, scalars)
                }
            }
        }
    }
}

public open class LeanCtor0(
    tag: Int
) : LeanCtor(tag, EMPTY_OBJS, EMPTY_SCALARS) {
    override val numObjs: Int get() = 0
    override val numScalars: Int get() = 0
    override fun hasScalars(): Boolean = false

    override fun getObj(index: Int): LeanObject? = null
    override fun setObj(index: Int, value: LeanObject?) {}
    override fun getObj0(): LeanObject? = null
    override fun setObj0(value: LeanObject?) {}
    override fun getObj1(): LeanObject? = null
    override fun setObj1(value: LeanObject?) {}

    override fun getScalar(offset: Int): Long = 0L
    override fun setScalar(offset: Int, value: Long) {}
    override fun getScalar0(): Long = 0L
    override fun setScalar0(value: Long) {}

    override fun toString(): String = "LeanCtor(tag=$tag, objs=[], scalars=[])"
}

public open class LeanCtor1(
    tag: Int,
    @JvmField public var obj0: LeanObject? = null
) : LeanCtor(tag, EMPTY_OBJS, EMPTY_SCALARS) {
    override val numObjs: Int get() = 1
    override val numScalars: Int get() = 0
    override fun hasScalars(): Boolean = false

    override fun getObj(index: Int): LeanObject? = if (index == 0) obj0 else null
    override fun setObj(index: Int, value: LeanObject?) { if (index == 0) obj0 = value }
    override fun getObj0(): LeanObject? = obj0
    override fun setObj0(value: LeanObject?) { obj0 = value }
    override fun getObj1(): LeanObject? = null
    override fun setObj1(value: LeanObject?) {}

    override fun getScalar(offset: Int): Long = 0L
    override fun setScalar(offset: Int, value: Long) {}
    override fun getScalar0(): Long = 0L
    override fun setScalar0(value: Long) {}

    override fun toString(): String = "LeanCtor(tag=$tag, objs=[$obj0], scalars=[])"
}

public open class LeanCtor2(
    tag: Int,
    @JvmField public var obj0: LeanObject? = null,
    @JvmField public var obj1: LeanObject? = null
) : LeanCtor(tag, EMPTY_OBJS, EMPTY_SCALARS) {
    override val numObjs: Int get() = 2
    override val numScalars: Int get() = 0
    override fun hasScalars(): Boolean = false

    override fun getObj(index: Int): LeanObject? = when (index) {
        0 -> obj0
        1 -> obj1
        else -> null
    }
    override fun setObj(index: Int, value: LeanObject?) {
        when (index) {
            0 -> obj0 = value
            1 -> obj1 = value
        }
    }
    override fun getObj0(): LeanObject? = obj0
    override fun setObj0(value: LeanObject?) { obj0 = value }
    override fun getObj1(): LeanObject? = obj1
    override fun setObj1(value: LeanObject?) { obj1 = value }

    override fun getScalar(offset: Int): Long = 0L
    override fun setScalar(offset: Int, value: Long) {}
    override fun getScalar0(): Long = 0L
    override fun setScalar0(value: Long) {}

    override fun toString(): String = "LeanCtor(tag=$tag, objs=[$obj0, $obj1], scalars=[])"
}

public open class LeanCtorScalar1(
    tag: Int,
    @JvmField public var scalar0: Long = 0L
) : LeanCtor(tag, EMPTY_OBJS, EMPTY_SCALARS) {
    override val numObjs: Int get() = 0
    override val numScalars: Int get() = 1
    override fun hasScalars(): Boolean = true

    override fun getObj(index: Int): LeanObject? = null
    override fun setObj(index: Int, value: LeanObject?) {}
    override fun getObj0(): LeanObject? = null
    override fun setObj0(value: LeanObject?) {}
    override fun getObj1(): LeanObject? = null
    override fun setObj1(value: LeanObject?) {}

    override fun getScalar(offset: Int): Long = scalar0
    override fun setScalar(offset: Int, value: Long) { scalar0 = value }
    override fun getScalar0(): Long = scalar0
    override fun setScalar0(value: Long) { scalar0 = value }

    override fun getByteScalar(n: Int, byteOffset: Int, numBytes: Int): Long =
        getByteScalar0(byteOffset, numBytes)

    override fun setByteScalar(n: Int, byteOffset: Int, numBytes: Int, value: Long) {
        setByteScalar0(byteOffset, numBytes, value)
    }

    override fun getByteScalar0(byteOffset: Int, numBytes: Int): Long {
        val shift = (byteOffset % 8) * 8
        return when (numBytes) {
            1 -> (scalar0 ushr shift) and 0xFFL
            2 -> (scalar0 ushr shift) and 0xFFFFL
            4 -> (scalar0 ushr shift) and 0xFFFFFFFFL
            else -> scalar0
        }
    }

    override fun setByteScalar0(byteOffset: Int, numBytes: Int, value: Long) {
        val shift = (byteOffset % 8) * 8
        if (numBytes >= 8) {
            scalar0 = value
            return
        }
        val mask = when (numBytes) {
            1 -> 0xFFL shl shift
            2 -> 0xFFFFL shl shift
            4 -> 0xFFFFFFFFL shl shift
            else -> -1L
        }
        val cleared = scalar0 and mask.inv()
        val maskedValue = when (numBytes) {
            1 -> value and 0xFFL
            2 -> value and 0xFFFFL
            4 -> value and 0xFFFFFFFFL
            else -> value
        }
        scalar0 = cleared or (maskedValue shl shift)
    }

    override fun toString(): String = "LeanCtor(tag=$tag, objs=[], scalars=[$scalar0])"
}
