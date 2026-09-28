/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
@file:Suppress(
    "ClassNaming",
    "Filename",
    "MatchingDeclarationName",
    "ReturnCount",
    "UnusedParameter",
)

package lean

import lean.runtime.LeanArray
import lean.runtime.LeanByteArray
import lean.runtime.LeanClosure
import lean.runtime.LeanCtor
import lean.runtime.LeanFloat
import lean.runtime.LeanNat
import lean.runtime.LeanObject
import lean.runtime.LeanRef
import lean.runtime.LeanString
import lean.runtime.LeanThunk
import kotlin.jvm.JvmStatic
import kotlin.math.abs
import kotlin.math.floor
import kotlin.math.log2
import kotlin.math.pow

internal fun toNat(obj: LeanObject?): LeanNat =
    when (obj) {
        is LeanNat -> obj
        null -> LeanNat.ZERO
        else -> LeanNat.ofLong(obj.tag.toLong())
    }

internal fun toULong(obj: LeanObject?): Long =
    when (obj) {
        is LeanNat -> obj.bigVal?.let { lean.runtime.bigIntToLong(it) } ?: obj.smallVal
        is LeanCtor -> {
            val nat = obj.getObj(0) as? LeanNat
            nat?.bigVal?.let { lean.runtime.bigIntToLong(it) } ?: nat?.smallVal ?: 0L
        }
        else -> (obj?.tag ?: 0).toLong()
    }

/**
 * Kotlin Multiplatform runtime primitives for Lean's standard `Init` library modules.
 */
public object mod_l_Init_Prelude {
    @JvmStatic
    public fun f_Array_mkEmpty(type: LeanObject?, capacity: LeanObject?): LeanObject = LeanArray.empty()

    @JvmStatic
    public fun f_Array_emptyWithCapacity(type: LeanObject?, capacity: LeanObject?): LeanObject = LeanArray.empty()

    @JvmStatic
    public fun f_ByteArray_mkEmpty(capacity: LeanObject?): LeanObject = LeanByteArray.empty()

    @JvmStatic
    public fun f_ByteArray_push(a: LeanObject?, b: LeanObject?): LeanObject {
        val ba = a as? LeanByteArray ?: LeanByteArray.empty()
        val byteVal = ((b as? LeanNat)?.smallVal ?: 0L).toByte()
        return ba.push(byteVal)
    }

    @JvmStatic
    public fun f_ByteArray_size(a: LeanObject?): LeanObject {
        val s = (a as? LeanByteArray)?.size() ?: 0
        return LeanNat.ofLong(s.toLong())
    }

    @JvmStatic
    public fun f_ByteArray_get(a: LeanObject?, i: LeanObject?): LeanObject {
        val ba = a as? LeanByteArray ?: return LeanNat.ZERO
        val idx = (i as? LeanNat)?.smallVal?.toInt() ?: 0
        return LeanNat.ofLong((ba.get(idx).toInt() and 0xFF).toLong())
    }

    @JvmStatic
    public fun f_ByteArray_getInternal(a: LeanObject?, i: LeanObject?): LeanObject = f_ByteArray_get(a, i)

    @JvmStatic
    public fun f_ByteArray_uget(a: LeanObject?, i: LeanObject?): LeanObject = f_ByteArray_get(a, i)

    @JvmStatic
    public fun f_ByteArray_fget(a: LeanObject?, i: LeanObject?): LeanObject = f_ByteArray_get(a, i)

    @JvmStatic
    public fun f_UInt64_toNat(a: LeanObject?): LeanObject = a ?: LeanNat.ZERO

    @JvmStatic
    public fun f_UInt32_toNat(a: LeanObject?): LeanObject = a ?: LeanNat.ZERO

    @JvmStatic
    public fun f_UInt16_toNat(a: LeanObject?): LeanObject = a ?: LeanNat.ZERO

    @JvmStatic
    public fun f_UInt8_toNat(a: LeanObject?): LeanObject = a ?: LeanNat.ZERO

    @JvmStatic
    public fun f_USize_toNat(a: LeanObject?): LeanObject = a ?: LeanNat.ZERO

    @JvmStatic
    public fun f_Array_push(type: LeanObject?, arr: LeanObject?, v: LeanObject?): LeanObject {
        return (arr as? LeanArray)?.push(v) ?: LeanArray.of(v)
    }

    @JvmStatic
    public fun f_Array_size(type: LeanObject?, arr: LeanObject?): LeanObject {
        val sz = (arr as? LeanArray)?.size()?.toLong() ?: 0L
        return LeanNat.ofLong(sz)
    }

    @JvmStatic
    public fun f_Array_getInternal(type: LeanObject?, arr: LeanObject?, i: LeanObject?, h: LeanObject?): LeanObject? {
        val idx = if (i is LeanNat) i.smallVal.toInt() else i?.tag ?: 0
        return (arr as? LeanArray)?.get(idx)
    }

    @JvmStatic
    public fun f_Array_getInternalBorrowed(type: LeanObject?, arr: LeanObject?, i: LeanObject?, h: LeanObject?): LeanObject? {
        val idx = if (i is LeanNat) i.smallVal.toInt() else i?.tag ?: 0
        return (arr as? LeanArray)?.get(idx)
    }

    @JvmStatic
    public fun f_Array_get_x21Internal(type: LeanObject?, inst: LeanObject?, arr: LeanObject?, i: LeanObject?): LeanObject? {
        val idx = if (i is LeanNat) i.smallVal.toInt() else i?.tag ?: 0
        return (arr as? LeanArray)?.get(idx)
    }

    @JvmStatic
    public fun f_Array_get_x21InternalBorrowed(type: LeanObject?, inst: LeanObject?, arr: LeanObject?, i: LeanObject?): LeanObject? {
        val idx = if (i is LeanNat) i.smallVal.toInt() else i?.tag ?: 0
        return (arr as? LeanArray)?.get(idx)
    }

    @JvmStatic
    public fun f_Array_mk(type: LeanObject?, list: LeanObject?): LeanObject {
        val elems = mutableListOf<LeanObject?>()
        var curr = list
        while (curr != null && curr.tag != 0) {
            val ctor = curr as? LeanCtor ?: break
            elems.add(ctor.getObj(0))
            curr = ctor.getObj(1)
        }
        val arr = arrayOfNulls<LeanObject>(elems.size)
        for (i in elems.indices) arr[i] = elems[i]
        return LeanArray(arr)
    }

    @JvmStatic
    public fun f_Array_toList(type: LeanObject?, arr: LeanObject?): LeanObject {
        val a = arr as? LeanArray ?: return LeanCtor(0)
        var list: LeanObject = LeanCtor(0)
        for (i in a.data.indices.reversed()) {
            list = LeanCtor(1, arrayOf(a.data[i], list))
        }
        return list
    }

    @JvmStatic
    public fun f_List_lengthTR___redArg(asObj: LeanObject?): LeanObject {
        var count = 0L
        var curr = asObj
        while (curr != null && curr.tag != 0) {
            val ctor = curr as? LeanCtor ?: break
            count++
            curr = ctor.getObj(1)
        }
        return LeanNat.ofLong(count)
    }
    @JvmStatic
    public fun f_Nat_add(a: LeanObject?, b: LeanObject?): LeanObject = toNat(a).add(toNat(b))

    @JvmStatic
    public fun f_Nat_sub(a: LeanObject?, b: LeanObject?): LeanObject = toNat(a).sub(toNat(b))

    @JvmStatic
    public fun f_Nat_mul(a: LeanObject?, b: LeanObject?): LeanObject = toNat(a).mul(toNat(b))

    @JvmStatic
    public fun f_Nat_div(a: LeanObject?, b: LeanObject?): LeanObject = toNat(a).div(toNat(b))

    @JvmStatic
    public fun f_Nat_mod(a: LeanObject?, b: LeanObject?): LeanObject = toNat(a).mod(toNat(b))

    @JvmStatic
    public fun f_Nat_pow(a: LeanObject?, b: LeanObject?): LeanObject = toNat(a).pow(toNat(b))

    @JvmStatic
    public fun f_Nat_gcd(a: LeanObject?, b: LeanObject?): LeanObject = toNat(a).gcd(toNat(b))

    @JvmStatic
    public fun f_Nat_decLe(a: LeanObject?, b: LeanObject?): LeanObject =
        if (toNat(a).le(toNat(b))) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_Nat_decEq(a: LeanObject?, b: LeanObject?): LeanObject =
        if (toNat(a).eq(toNat(b))) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_Nat_decLt(a: LeanObject?, b: LeanObject?): LeanObject =
        if (toNat(a).lt(toNat(b))) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_Nat_ble(a: LeanObject?, b: LeanObject?): LeanObject = f_Nat_decLe(a, b)

    @JvmStatic
    public fun f_Nat_blt(a: LeanObject?, b: LeanObject?): LeanObject = f_Nat_decLt(a, b)

    @JvmStatic
    public fun f_Nat_beq(a: LeanObject?, b: LeanObject?): LeanObject = f_Nat_decEq(a, b)

    @JvmStatic
    public fun f_UInt64_decEq(a: LeanObject?, b: LeanObject?): LeanObject =
        if (toULong(a) == toULong(b)) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_UInt32_decEq(a: LeanObject?, b: LeanObject?): LeanObject =
        if ((toULong(a) and 0xFFFFFFFFL) == (toULong(b) and 0xFFFFFFFFL)) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_UInt16_decEq(a: LeanObject?, b: LeanObject?): LeanObject =
        if ((toULong(a) and 0xFFFFL) == (toULong(b) and 0xFFFFL)) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_UInt8_decEq(a: LeanObject?, b: LeanObject?): LeanObject =
        if ((toULong(a) and 0xFFL) == (toULong(b) and 0xFFL)) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_USize_decEq(a: LeanObject?, b: LeanObject?): LeanObject =
        if (toULong(a) == toULong(b)) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_USize_decLt(a: LeanObject?, b: LeanObject?): LeanObject =
        if (toULong(a).toULong() < toULong(b).toULong()) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_USize_decLe(a: LeanObject?, b: LeanObject?): LeanObject =
        if (toULong(a).toULong() <= toULong(b).toULong()) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_String_decEq(a: LeanObject?, b: LeanObject?): LeanObject {
        val s1 = a?.toString() ?: ""
        val s2 = b?.toString() ?: ""
        return if (s1 == s2) LeanNat.ONE else LeanNat.ZERO
    }
}

public object mod_l_Init_Data_UInt_Basic {
    // UInt8
    @JvmStatic public fun f_UInt8_ofNat(a: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) and 0xFFL)
    @JvmStatic public fun f_UInt8_toNat(a: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) and 0xFFL)
    @JvmStatic public fun f_UInt8_add(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) + toULong(b)) and 0xFFL)
    @JvmStatic public fun f_UInt8_sub(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) - toULong(b)) and 0xFFL)
    @JvmStatic public fun f_UInt8_mul(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) * toULong(b)) and 0xFFL)
    @JvmStatic public fun f_UInt8_div(a: LeanObject?, b: LeanObject?): LeanObject {
        val bVal = toULong(b) and 0xFFL
        return if (bVal == 0L) LeanNat.ZERO else LeanNat.ofLong(((toULong(a) and 0xFFL) / bVal) and 0xFFL)
    }
    @JvmStatic public fun f_UInt8_mod(a: LeanObject?, b: LeanObject?): LeanObject {
        val bVal = toULong(b) and 0xFFL
        return if (bVal == 0L) LeanNat.ofLong(toULong(a) and 0xFFL) else LeanNat.ofLong(((toULong(a) and 0xFFL) % bVal) and 0xFFL)
    }
    @JvmStatic public fun f_UInt8_land(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) and toULong(b)) and 0xFFL)
    @JvmStatic public fun f_UInt8_lor(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) or toULong(b)) and 0xFFL)
    @JvmStatic public fun f_UInt8_xor(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) xor toULong(b)) and 0xFFL)
    @JvmStatic public fun f_UInt8_shiftLeft(a: LeanObject?, b: LeanObject?): LeanObject {
        val shift = ((toULong(b) and 0xFFL) % 8L).toInt()
        return LeanNat.ofLong(((toULong(a) and 0xFFL) shl shift) and 0xFFL)
    }
    @JvmStatic public fun f_UInt8_shiftRight(a: LeanObject?, b: LeanObject?): LeanObject {
        val shift = ((toULong(b) and 0xFFL) % 8L).toInt()
        return LeanNat.ofLong(((toULong(a) and 0xFFL) ushr shift) and 0xFFL)
    }
    @JvmStatic public fun f_UInt8_complement(a: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a).inv()) and 0xFFL)
    @JvmStatic public fun f_UInt8_neg(a: LeanObject?): LeanObject = LeanNat.ofLong((-toULong(a)) and 0xFFL)
    @JvmStatic public fun f_UInt8_decEq(a: LeanObject?, b: LeanObject?): LeanObject =
        if ((toULong(a) and 0xFFL) == (toULong(b) and 0xFFL)) LeanNat.ONE else LeanNat.ZERO
    @JvmStatic public fun f_UInt8_decLt(a: LeanObject?, b: LeanObject?): LeanObject =
        if ((toULong(a) and 0xFFL) < (toULong(b) and 0xFFL)) LeanNat.ONE else LeanNat.ZERO
    @JvmStatic public fun f_UInt8_decLe(a: LeanObject?, b: LeanObject?): LeanObject =
        if ((toULong(a) and 0xFFL) <= (toULong(b) and 0xFFL)) LeanNat.ONE else LeanNat.ZERO

    // UInt16
    @JvmStatic public fun f_UInt16_ofNat(a: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) and 0xFFFFL)
    @JvmStatic public fun f_UInt16_toNat(a: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) and 0xFFFFL)
    @JvmStatic public fun f_UInt16_add(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) + toULong(b)) and 0xFFFFL)
    @JvmStatic public fun f_UInt16_sub(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) - toULong(b)) and 0xFFFFL)
    @JvmStatic public fun f_UInt16_mul(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) * toULong(b)) and 0xFFFFL)
    @JvmStatic public fun f_UInt16_div(a: LeanObject?, b: LeanObject?): LeanObject {
        val bVal = toULong(b) and 0xFFFFL
        return if (bVal == 0L) LeanNat.ZERO else LeanNat.ofLong(((toULong(a) and 0xFFFFL) / bVal) and 0xFFFFL)
    }
    @JvmStatic public fun f_UInt16_mod(a: LeanObject?, b: LeanObject?): LeanObject {
        val bVal = toULong(b) and 0xFFFFL
        return if (bVal == 0L) LeanNat.ofLong(toULong(a) and 0xFFFFL) else LeanNat.ofLong(((toULong(a) and 0xFFFFL) % bVal) and 0xFFFFL)
    }
    @JvmStatic public fun f_UInt16_land(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) and toULong(b)) and 0xFFFFL)
    @JvmStatic public fun f_UInt16_lor(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) or toULong(b)) and 0xFFFFL)
    @JvmStatic public fun f_UInt16_xor(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) xor toULong(b)) and 0xFFFFL)
    @JvmStatic public fun f_UInt16_shiftLeft(a: LeanObject?, b: LeanObject?): LeanObject {
        val shift = ((toULong(b) and 0xFFFFL) % 16L).toInt()
        return LeanNat.ofLong(((toULong(a) and 0xFFFFL) shl shift) and 0xFFFFL)
    }
    @JvmStatic public fun f_UInt16_shiftRight(a: LeanObject?, b: LeanObject?): LeanObject {
        val shift = ((toULong(b) and 0xFFFFL) % 16L).toInt()
        return LeanNat.ofLong(((toULong(a) and 0xFFFFL) ushr shift) and 0xFFFFL)
    }
    @JvmStatic public fun f_UInt16_complement(a: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a).inv()) and 0xFFFFL)
    @JvmStatic public fun f_UInt16_neg(a: LeanObject?): LeanObject = LeanNat.ofLong((-toULong(a)) and 0xFFFFL)
    @JvmStatic public fun f_UInt16_decEq(a: LeanObject?, b: LeanObject?): LeanObject =
        if ((toULong(a) and 0xFFFFL) == (toULong(b) and 0xFFFFL)) LeanNat.ONE else LeanNat.ZERO
    @JvmStatic public fun f_UInt16_decLt(a: LeanObject?, b: LeanObject?): LeanObject =
        if ((toULong(a) and 0xFFFFL) < (toULong(b) and 0xFFFFL)) LeanNat.ONE else LeanNat.ZERO
    @JvmStatic public fun f_UInt16_decLe(a: LeanObject?, b: LeanObject?): LeanObject =
        if ((toULong(a) and 0xFFFFL) <= (toULong(b) and 0xFFFFL)) LeanNat.ONE else LeanNat.ZERO

    // UInt32
    @JvmStatic public fun f_UInt32_ofNat(a: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) and 0xFFFFFFFFL)
    @JvmStatic public fun f_UInt32_toNat(a: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) and 0xFFFFFFFFL)
    @JvmStatic public fun f_UInt32_add(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) + toULong(b)) and 0xFFFFFFFFL)
    @JvmStatic public fun f_UInt32_sub(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) - toULong(b)) and 0xFFFFFFFFL)
    @JvmStatic public fun f_UInt32_mul(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) * toULong(b)) and 0xFFFFFFFFL)
    @JvmStatic public fun f_UInt32_div(a: LeanObject?, b: LeanObject?): LeanObject {
        val bVal = toULong(b) and 0xFFFFFFFFL
        return if (bVal == 0L) LeanNat.ZERO else LeanNat.ofLong(((toULong(a) and 0xFFFFFFFFL) / bVal) and 0xFFFFFFFFL)
    }
    @JvmStatic public fun f_UInt32_mod(a: LeanObject?, b: LeanObject?): LeanObject {
        val bVal = toULong(b) and 0xFFFFFFFFL
        return if (bVal == 0L) LeanNat.ofLong(toULong(a) and 0xFFFFFFFFL) else LeanNat.ofLong(((toULong(a) and 0xFFFFFFFFL) % bVal) and 0xFFFFFFFFL)
    }
    @JvmStatic public fun f_UInt32_land(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) and toULong(b)) and 0xFFFFFFFFL)
    @JvmStatic public fun f_UInt32_lor(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) or toULong(b)) and 0xFFFFFFFFL)
    @JvmStatic public fun f_UInt32_xor(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a) xor toULong(b)) and 0xFFFFFFFFL)
    @JvmStatic public fun f_UInt32_shiftLeft(a: LeanObject?, b: LeanObject?): LeanObject {
        val shift = ((toULong(b) and 0xFFFFFFFFL) % 32L).toInt()
        return LeanNat.ofLong(((toULong(a) and 0xFFFFFFFFL) shl shift) and 0xFFFFFFFFL)
    }
    @JvmStatic public fun f_UInt32_shiftRight(a: LeanObject?, b: LeanObject?): LeanObject {
        val shift = ((toULong(b) and 0xFFFFFFFFL) % 32L).toInt()
        return LeanNat.ofLong(((toULong(a) and 0xFFFFFFFFL) ushr shift) and 0xFFFFFFFFL)
    }
    @JvmStatic public fun f_UInt32_complement(a: LeanObject?): LeanObject = LeanNat.ofLong((toULong(a).inv()) and 0xFFFFFFFFL)
    @JvmStatic public fun f_UInt32_neg(a: LeanObject?): LeanObject = LeanNat.ofLong((-toULong(a)) and 0xFFFFFFFFL)
    @JvmStatic public fun f_UInt32_decEq(a: LeanObject?, b: LeanObject?): LeanObject =
        if ((toULong(a) and 0xFFFFFFFFL) == (toULong(b) and 0xFFFFFFFFL)) LeanNat.ONE else LeanNat.ZERO
    @JvmStatic public fun f_UInt32_decLt(a: LeanObject?, b: LeanObject?): LeanObject =
        if ((toULong(a) and 0xFFFFFFFFL) < (toULong(b) and 0xFFFFFFFFL)) LeanNat.ONE else LeanNat.ZERO
    @JvmStatic public fun f_UInt32_decLe(a: LeanObject?, b: LeanObject?): LeanObject =
        if ((toULong(a) and 0xFFFFFFFFL) <= (toULong(b) and 0xFFFFFFFFL)) LeanNat.ONE else LeanNat.ZERO

    // UInt64
    @JvmStatic public fun f_UInt64_ofNat(a: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a))
    @JvmStatic public fun f_UInt64_toNat(a: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a))
    @JvmStatic public fun f_UInt64_add(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) + toULong(b))
    @JvmStatic public fun f_UInt64_sub(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) - toULong(b))
    @JvmStatic public fun f_UInt64_mul(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) * toULong(b))
    @JvmStatic public fun f_UInt64_div(a: LeanObject?, b: LeanObject?): LeanObject {
        val bVal = toULong(b).toULong()
        return if (bVal == 0UL) LeanNat.ZERO else LeanNat.ofLong((toULong(a).toULong() / bVal).toLong())
    }
    @JvmStatic public fun f_UInt64_mod(a: LeanObject?, b: LeanObject?): LeanObject {
        val bVal = toULong(b).toULong()
        return if (bVal == 0UL) LeanNat.ofLong(toULong(a)) else LeanNat.ofLong((toULong(a).toULong() % bVal).toLong())
    }
    @JvmStatic public fun f_UInt64_land(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) and toULong(b))
    @JvmStatic public fun f_UInt64_lor(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) or toULong(b))
    @JvmStatic public fun f_UInt64_xor(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) xor toULong(b))
    @JvmStatic public fun f_UInt64_shiftLeft(a: LeanObject?, b: LeanObject?): LeanObject {
        val shift = (toULong(b).toULong() % 64UL).toInt()
        return LeanNat.ofLong(toULong(a) shl shift)
    }
    @JvmStatic public fun f_UInt64_shiftRight(a: LeanObject?, b: LeanObject?): LeanObject {
        val shift = (toULong(b).toULong() % 64UL).toInt()
        return LeanNat.ofLong(toULong(a) ushr shift)
    }
    @JvmStatic public fun f_UInt64_complement(a: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a).inv())
    @JvmStatic public fun f_UInt64_neg(a: LeanObject?): LeanObject = LeanNat.ofLong(-toULong(a))
    @JvmStatic public fun f_UInt64_decEq(a: LeanObject?, b: LeanObject?): LeanObject =
        if (toULong(a) == toULong(b)) LeanNat.ONE else LeanNat.ZERO
    @JvmStatic public fun f_UInt64_decLt(a: LeanObject?, b: LeanObject?): LeanObject =
        if (toULong(a).toULong() < toULong(b).toULong()) LeanNat.ONE else LeanNat.ZERO
    @JvmStatic public fun f_UInt64_decLe(a: LeanObject?, b: LeanObject?): LeanObject =
        if (toULong(a).toULong() <= toULong(b).toULong()) LeanNat.ONE else LeanNat.ZERO

    // USize
    @JvmStatic public fun f_USize_ofNat(a: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a))
    @JvmStatic public fun f_USize_toNat(a: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a))
    @JvmStatic public fun f_USize_add(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) + toULong(b))
    @JvmStatic public fun f_USize_sub(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) - toULong(b))
    @JvmStatic public fun f_USize_mul(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) * toULong(b))
    @JvmStatic public fun f_USize_div(a: LeanObject?, b: LeanObject?): LeanObject {
        val bVal = toULong(b).toULong()
        return if (bVal == 0UL) LeanNat.ZERO else LeanNat.ofLong((toULong(a).toULong() / bVal).toLong())
    }
    @JvmStatic public fun f_USize_mod(a: LeanObject?, b: LeanObject?): LeanObject {
        val bVal = toULong(b).toULong()
        return if (bVal == 0UL) LeanNat.ofLong(toULong(a)) else LeanNat.ofLong((toULong(a).toULong() % bVal).toLong())
    }
    @JvmStatic public fun f_USize_land(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) and toULong(b))
    @JvmStatic public fun f_USize_lor(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) or toULong(b))
    @JvmStatic public fun f_USize_xor(a: LeanObject?, b: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a) xor toULong(b))
    @JvmStatic public fun f_USize_shiftLeft(a: LeanObject?, b: LeanObject?): LeanObject {
        val shift = (toULong(b).toULong() % 64UL).toInt()
        return LeanNat.ofLong(toULong(a) shl shift)
    }
    @JvmStatic public fun f_USize_shiftRight(a: LeanObject?, b: LeanObject?): LeanObject {
        val shift = (toULong(b).toULong() % 64UL).toInt()
        return LeanNat.ofLong(toULong(a) ushr shift)
    }
    @JvmStatic public fun f_USize_complement(a: LeanObject?): LeanObject = LeanNat.ofLong(toULong(a).inv())
    @JvmStatic public fun f_USize_neg(a: LeanObject?): LeanObject = LeanNat.ofLong(-toULong(a))
    @JvmStatic public fun f_USize_decEq(a: LeanObject?, b: LeanObject?): LeanObject =
        if (toULong(a) == toULong(b)) LeanNat.ONE else LeanNat.ZERO
    @JvmStatic public fun f_USize_decLt(a: LeanObject?, b: LeanObject?): LeanObject =
        if (toULong(a).toULong() < toULong(b).toULong()) LeanNat.ONE else LeanNat.ZERO
    @JvmStatic public fun f_USize_decLe(a: LeanObject?, b: LeanObject?): LeanObject =
        if (toULong(a).toULong() <= toULong(b).toULong()) LeanNat.ONE else LeanNat.ZERO
}

public object mod_l_Init_Data_UInt_BasicAux {
    @JvmStatic public fun f_UInt64_ofNat(a: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_UInt64_ofNat(a)
    @JvmStatic public fun f_UInt64_toNat(a: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_UInt64_toNat(a)
    @JvmStatic public fun f_UInt32_ofNat(a: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_UInt32_ofNat(a)
    @JvmStatic public fun f_UInt32_toNat(a: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_UInt32_toNat(a)
    @JvmStatic public fun f_UInt16_ofNat(a: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_UInt16_ofNat(a)
    @JvmStatic public fun f_UInt16_toNat(a: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_UInt16_toNat(a)
    @JvmStatic public fun f_UInt8_ofNat(a: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_UInt8_ofNat(a)
    @JvmStatic public fun f_UInt8_toNat(a: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_UInt8_toNat(a)
    @JvmStatic public fun f_USize_ofNat(a: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_USize_ofNat(a)
    @JvmStatic public fun f_USize_toNat(a: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_USize_toNat(a)
    @JvmStatic public fun f_USize_add(a: LeanObject?, b: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_USize_add(a, b)
    @JvmStatic public fun f_USize_sub(a: LeanObject?, b: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_USize_sub(a, b)
    @JvmStatic public fun f_USize_mul(a: LeanObject?, b: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_USize_mul(a, b)
    @JvmStatic public fun f_USize_div(a: LeanObject?, b: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_USize_div(a, b)
    @JvmStatic public fun f_USize_mod(a: LeanObject?, b: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_USize_mod(a, b)
    @JvmStatic public fun f_USize_decEq(a: LeanObject?, b: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_USize_decEq(a, b)
    @JvmStatic public fun f_USize_decLt(a: LeanObject?, b: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_USize_decLt(a, b)
    @JvmStatic public fun f_USize_decLe(a: LeanObject?, b: LeanObject?): LeanObject = mod_l_Init_Data_UInt_Basic.f_USize_decLe(a, b)
}

public object mod_l_Init_Data_Option_Basic {
    @JvmStatic
    public fun f_Option_instDecidableEq___redArg(eqDec: LeanObject?, a: LeanObject?, b: LeanObject?): LeanObject {
        if (a == null && b == null) return LeanNat.ONE
        if (a == null || b == null) return LeanNat.ZERO
        val aCtor = a as? LeanCtor ?: return LeanNat.ZERO
        val bCtor = b as? LeanCtor ?: return LeanNat.ZERO
        if (aCtor.tag != bCtor.tag) return LeanNat.ZERO
        if (aCtor.tag == 0) return LeanNat.ONE // none == none
        val aVal = aCtor.getObj(0)
        val bVal = bCtor.getObj(0)
        return if (aVal == bVal) LeanNat.ONE else LeanNat.ZERO
    }

    @JvmStatic
    public fun f_Option_isSome(a: LeanObject?): LeanObject =
        if (a is LeanCtor && a.tag == 1) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_Option_isNone(a: LeanObject?): LeanObject =
        if (a is LeanCtor && a.tag == 0) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_Option_getD(a: LeanObject?, fallback: LeanObject?): LeanObject? =
        if (a is LeanCtor && a.tag == 1) a.getObj(0) else fallback
}

public object mod_l_Init_Data_String_Bootstrap {
    @JvmStatic
    public fun f_String_Internal_length(s: LeanObject?): LeanObject {
        val len = if (s is LeanString) s.byteSize.toLong() else 0L
        return LeanNat.ofLong(len)
    }

    @JvmStatic
    public fun f_String_push(str: LeanObject?, c: LeanObject?): LeanObject {
        val base = if (str is LeanString) str.toString() else ""
        val codePoint = if (c is LeanNat) c.smallVal.toInt() else c?.tag ?: 0
        return LeanString.of(base + String(Character.toChars(codePoint)))
    }

    @JvmStatic
    public fun f_String_append(a: LeanObject?, b: LeanObject?): LeanObject {
        if (a is LeanString && b is LeanString) {
            return LeanString.concat(a, b)
        }
        return LeanString.of((a?.toString() ?: "") + (b?.toString() ?: ""))
    }

    @JvmStatic
    public fun f_String_isEmpty(s: LeanObject?): LeanObject =
        if (s is LeanString) {
            if (s.byteSize == 0) LeanNat.ONE else LeanNat.ZERO
        } else LeanNat.ONE
}

public object mod_l_Init_Data_Int_Basic {
    internal fun getBigIntVal(obj: LeanObject?): lean.runtime.LeanBigInt {
        if (obj is LeanNat) return obj.toBigInteger()
        if (obj is LeanCtor) {
            val nat = (obj.getObj(0) as? LeanNat)?.toBigInteger() ?: lean.runtime.createBigInt(0L)
            return if (obj.tag == 0) {
                nat
            } else {
                lean.runtime.bigIntNeg(lean.runtime.bigIntAdd(nat, lean.runtime.createBigInt(1L)))
            }
        }
        val tag = obj?.tag ?: 0
        return lean.runtime.createBigInt(tag.toLong())
    }

    internal fun toLeanInt(v: lean.runtime.LeanBigInt): LeanObject {
        val sign = lean.runtime.bigIntSignum(v)
        return if (sign >= 0) {
            val nat = LeanNat(v)
            val res = LeanCtor.alloc(0, 1, 0)
            res.setObj(0, nat)
            res
        } else {
            val abs = lean.runtime.bigIntAbs(v)
            val pred = lean.runtime.bigIntSub(abs, lean.runtime.createBigInt(1L))
            val nat = LeanNat(pred)
            val res = LeanCtor.alloc(1, 1, 0)
            res.setObj(0, nat)
            res
        }
    }

    @JvmStatic
    public fun f_Int_ofNat(n: LeanObject?): LeanObject {
        val nat = toNat(n)
        val res = LeanCtor.alloc(0, 1, 0)
        res.setObj(0, nat)
        return res
    }

    @JvmStatic
    public fun f_Int_negSucc(n: LeanObject?): LeanObject {
        val nat = toNat(n)
        val res = LeanCtor.alloc(1, 1, 0)
        res.setObj(0, nat)
        return res
    }

    @JvmStatic
    public fun f_Int_natAbs(n: LeanObject?): LeanObject {
        if (n is LeanCtor) {
            val natVal = (n.getObj(0) as? LeanNat) ?: LeanNat.ZERO
            return if (n.tag == 0) natVal else natVal.add(LeanNat.ONE)
        }
        if (n is LeanNat) {
            return n
        }
        return LeanNat.ZERO
    }

    @JvmStatic
    public fun f_Int_neg(n: LeanObject?): LeanObject {
        val v = getBigIntVal(n)
        return toLeanInt(lean.runtime.bigIntNeg(v))
    }

    @JvmStatic
    public fun f_Int_negOfNat(n: LeanObject?): LeanObject {
        val nat = toNat(n)
        if (nat.isZero()) return f_Int_ofNat(nat)
        val pred = nat.sub(LeanNat.ONE)
        return f_Int_negSucc(pred)
    }

    @JvmStatic
    public fun f_Int_add(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = getBigIntVal(a)
        val bVal = getBigIntVal(b)
        return toLeanInt(lean.runtime.bigIntAdd(aVal, bVal))
    }

    @JvmStatic
    public fun f_Int_sub(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = getBigIntVal(a)
        val bVal = getBigIntVal(b)
        return toLeanInt(lean.runtime.bigIntSub(aVal, bVal))
    }

    @JvmStatic
    public fun f_Int_mul(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = getBigIntVal(a)
        val bVal = getBigIntVal(b)
        return toLeanInt(lean.runtime.bigIntMul(aVal, bVal))
    }

    @JvmStatic
    public fun f_Int_div(a: LeanObject?, b: LeanObject?): LeanObject {
        val bVal = getBigIntVal(b)
        if (lean.runtime.bigIntIsZero(bVal)) return toLeanInt(lean.runtime.createBigInt(0L))
        val aVal = getBigIntVal(a)
        return toLeanInt(lean.runtime.bigIntDiv(aVal, bVal))
    }

    @JvmStatic
    public fun f_Int_mod(a: LeanObject?, b: LeanObject?): LeanObject {
        val bVal = getBigIntVal(b)
        val aVal = getBigIntVal(a)
        if (lean.runtime.bigIntIsZero(bVal)) return toLeanInt(aVal)
        return toLeanInt(lean.runtime.bigIntMod(aVal, bVal))
    }

    @JvmStatic
    public fun f_Int_ediv(a: LeanObject?, b: LeanObject?): LeanObject {
        val bVal = getBigIntVal(b)
        if (lean.runtime.bigIntIsZero(bVal)) return toLeanInt(lean.runtime.createBigInt(0L))
        val aVal = getBigIntVal(a)
        val q = lean.runtime.bigIntDiv(aVal, bVal)
        val r = lean.runtime.bigIntMod(aVal, bVal)
        if (lean.runtime.bigIntSignum(r) < 0) {
            return if (lean.runtime.bigIntSignum(bVal) > 0) {
                toLeanInt(lean.runtime.bigIntSub(q, lean.runtime.createBigInt(1L)))
            } else {
                toLeanInt(lean.runtime.bigIntAdd(q, lean.runtime.createBigInt(1L)))
            }
        }
        return toLeanInt(q)
    }

    @JvmStatic
    public fun f_Int_emod(a: LeanObject?, b: LeanObject?): LeanObject {
        val bVal = getBigIntVal(b)
        val aVal = getBigIntVal(a)
        if (lean.runtime.bigIntIsZero(bVal)) return toLeanInt(aVal)
        val r = lean.runtime.bigIntMod(aVal, bVal)
        if (lean.runtime.bigIntSignum(r) < 0) {
            return if (lean.runtime.bigIntSignum(bVal) > 0) {
                toLeanInt(lean.runtime.bigIntAdd(r, bVal))
            } else {
                toLeanInt(lean.runtime.bigIntSub(r, bVal))
            }
        }
        return toLeanInt(r)
    }

    @JvmStatic
    public fun f_Int_decEq(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = getBigIntVal(a)
        val bVal = getBigIntVal(b)
        return if (lean.runtime.bigIntCompare(aVal, bVal) == 0) LeanNat.ONE else LeanNat.ZERO
    }

    @JvmStatic
    public fun f_Int_decLe(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = getBigIntVal(a)
        val bVal = getBigIntVal(b)
        return if (lean.runtime.bigIntCompare(aVal, bVal) <= 0) LeanNat.ONE else LeanNat.ZERO
    }

    @JvmStatic
    public fun f_Int_decLt(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = getBigIntVal(a)
        val bVal = getBigIntVal(b)
        return if (lean.runtime.bigIntCompare(aVal, bVal) < 0) LeanNat.ONE else LeanNat.ZERO
    }
}

public object mod_l_Init_Data_Int_DivMod_Basic {
    @JvmStatic public fun f_Int_div(a: LeanObject?, b: LeanObject?): LeanObject = mod_l_Init_Data_Int_Basic.f_Int_div(a, b)
    @JvmStatic public fun f_Int_mod(a: LeanObject?, b: LeanObject?): LeanObject = mod_l_Init_Data_Int_Basic.f_Int_mod(a, b)
    @JvmStatic public fun f_Int_ediv(a: LeanObject?, b: LeanObject?): LeanObject = mod_l_Init_Data_Int_Basic.f_Int_ediv(a, b)
    @JvmStatic public fun f_Int_emod(a: LeanObject?, b: LeanObject?): LeanObject = mod_l_Init_Data_Int_Basic.f_Int_emod(a, b)
    @JvmStatic public fun f_Int_divExact(a: LeanObject?, b: LeanObject?, h: LeanObject?): LeanObject = mod_l_Init_Data_Int_Basic.f_Int_ediv(a, b)
}

public object mod_l_Init_Data_Nat_Bitwise_Basic {
    @JvmStatic
    public fun f_Nat_shiftRight(a: LeanObject?, b: LeanObject?): LeanObject {
        if (a is LeanNat && b is LeanNat) {
            val bVal = b.bigVal?.let { lean.runtime.bigIntToLong(it) } ?: b.smallVal
            if (bVal < 0 || bVal > Int.MAX_VALUE) return LeanNat.ZERO
            return LeanNat.ofBigInteger(lean.runtime.bigIntShiftRight(a.toBigInteger(), bVal.toInt()))
        }
        val aVal = if (a is LeanNat) a.smallVal else (a?.tag?.toLong() ?: 0L)
        val bVal = if (b is LeanNat) b.smallVal else (b?.tag?.toLong() ?: 0L)
        if (bVal >= 64L) return LeanNat.ZERO
        return LeanNat.ofLong(aVal ushr bVal.toInt())
    }

    @JvmStatic
    public fun f_Nat_shiftLeft(a: LeanObject?, b: LeanObject?): LeanObject {
        if (a is LeanNat && b is LeanNat) {
            val bVal = b.bigVal?.let { lean.runtime.bigIntToLong(it) } ?: b.smallVal
            if (bVal < 0 || bVal > 1_000_000) return LeanNat.ZERO
            return LeanNat.ofBigInteger(lean.runtime.bigIntShiftLeft(a.toBigInteger(), bVal.toInt()))
        }
        val aVal = if (a is LeanNat) a.smallVal else (a?.tag?.toLong() ?: 0L)
        val bVal = if (b is LeanNat) b.smallVal else (b?.tag?.toLong() ?: 0L)
        if (bVal >= 64L) return LeanNat.ZERO
        return LeanNat.ofLong(aVal shl bVal.toInt())
    }

    @JvmStatic
    public fun f_Nat_land(a: LeanObject?, b: LeanObject?): LeanObject {
        if (a is LeanNat && b is LeanNat) {
            return LeanNat.ofBigInteger(lean.runtime.bigIntAnd(a.toBigInteger(), b.toBigInteger()))
        }
        val aVal = if (a is LeanNat) a.smallVal else 0L
        val bVal = if (b is LeanNat) b.smallVal else 0L
        return LeanNat.ofLong(aVal and bVal)
    }

    @JvmStatic
    public fun f_Nat_lor(a: LeanObject?, b: LeanObject?): LeanObject {
        if (a is LeanNat && b is LeanNat) {
            return LeanNat.ofBigInteger(lean.runtime.bigIntOr(a.toBigInteger(), b.toBigInteger()))
        }
        val aVal = if (a is LeanNat) a.smallVal else 0L
        val bVal = if (b is LeanNat) b.smallVal else 0L
        return LeanNat.ofLong(aVal or bVal)
    }

    @JvmStatic
    public fun f_Nat_xor(a: LeanObject?, b: LeanObject?): LeanObject {
        if (a is LeanNat && b is LeanNat) {
            return LeanNat.ofBigInteger(lean.runtime.bigIntXor(a.toBigInteger(), b.toBigInteger()))
        }
        val aVal = if (a is LeanNat) a.smallVal else 0L
        val bVal = if (b is LeanNat) b.smallVal else 0L
        return LeanNat.ofLong(aVal xor bVal)
    }
}

public object mod_l_Init_Data_Repr {
    @JvmStatic
    public fun f_Repr_addAppParen(a: LeanObject?, b: LeanObject?): LeanObject = a ?: LeanString.of("")

    @JvmStatic
    public fun f_Bool_repr___redArg(b: LeanObject?): LeanObject {
        val isTrue = if (b is LeanNat) !b.isZero() else ((b?.tag ?: 0) != 0)
        return LeanString.of(if (isTrue) "true" else "false")
    }

    @JvmStatic
    public fun f_Nat_reprFast(n: LeanObject?): LeanObject {
        val v = if (n is LeanNat) n.toString() else (n?.tag ?: 0).toString()
        return LeanString.of(v)
    }

    @JvmStatic
    public fun f_Option_repr___redArg(inst: LeanObject?, opt: LeanObject?, prec: LeanObject?): LeanObject =
        LeanString.of(opt?.toString() ?: "none")
}

public object mod_l_Init_Data_Array_Basic {
    @JvmStatic
    public fun f_Array_swap(type: LeanObject?, arr: LeanObject?, i: LeanObject?, j: LeanObject?, h1: LeanObject?, h2: LeanObject?): LeanObject {
        val a = arr as? LeanArray ?: return LeanArray.empty()
        val idxI = if (i is LeanNat) i.smallVal.toInt() else i?.tag ?: 0
        val idxJ = if (j is LeanNat) j.smallVal.toInt() else j?.tag ?: 0
        val copy = a.data.copyOf()
        val tmp = copy[idxI]
        copy[idxI] = copy[idxJ]
        copy[idxJ] = tmp
        return LeanArray(copy)
    }

    @JvmStatic
    public fun f_Array_fswap(type: LeanObject?, arr: LeanObject?, i: LeanObject?, j: LeanObject?, h1: LeanObject?, h2: LeanObject?): LeanObject =
        f_Array_swap(type, arr, i, j, h1, h2)

    @JvmStatic
    public fun f_Array_uget(type: LeanObject?, arr: LeanObject?, usize: LeanObject?, h: LeanObject?): LeanObject? {
        val idx = (usize as? LeanNat)?.smallVal?.toInt() ?: usize?.tag ?: 0
        return (arr as? LeanArray)?.get(idx)
    }

    @JvmStatic
    public fun f_Array_ugetBorrowed(type: LeanObject?, arr: LeanObject?, usize: LeanObject?, h: LeanObject?): LeanObject? {
        val idx = (usize as? LeanNat)?.smallVal?.toInt() ?: usize?.tag ?: 0
        return (arr as? LeanArray)?.get(idx)
    }

    @JvmStatic
    public fun f_Array_uget_borrowed(type: LeanObject?, arr: LeanObject?, usize: LeanObject?): LeanObject? {
        val idx = (usize as? LeanNat)?.smallVal?.toInt() ?: usize?.tag ?: 0
        return (arr as? LeanArray)?.get(idx)
    }

    @JvmStatic
    public fun f_Array_uset(type: LeanObject?, arr: LeanObject?, usize: LeanObject?, v: LeanObject?, h: LeanObject?): LeanObject {
        val a = arr as? LeanArray ?: return LeanArray.empty()
        val idx = (usize as? LeanNat)?.smallVal?.toInt() ?: usize?.tag ?: 0
        val copy = a.data.copyOf()
        copy[idx] = v
        return LeanArray(copy)
    }

    @JvmStatic
    public fun f_Array_pop(type: LeanObject?, arr: LeanObject?): LeanObject {
        val a = arr as? LeanArray ?: return LeanArray.empty()
        if (a.data.isEmpty()) return a
        return LeanArray(a.data.copyOf(a.data.size - 1))
    }
}

public object mod_l_Init_Data_Array_Set {
    @JvmStatic
    public fun f_Array_set(type: LeanObject?, arr: LeanObject?, i: LeanObject?, v: LeanObject?, h: LeanObject?): LeanObject {
        val a = arr as? LeanArray ?: return LeanArray.empty()
        val idx = if (i is LeanNat) i.smallVal.toInt() else i?.tag ?: 0
        val copy = a.data.copyOf()
        copy[idx] = v
        return LeanArray(copy)
    }

    @JvmStatic
    public fun f_Array_fset(type: LeanObject?, arr: LeanObject?, i: LeanObject?, v: LeanObject?, h: LeanObject?): LeanObject =
        f_Array_set(type, arr, i, v, h)
}

public object mod_l_Init_Data_List_Basic {
    @JvmStatic
    public fun f_List_replicateTR___redArg(n: LeanObject?, elem: LeanObject?): LeanObject {
        val count = (n as? LeanNat)?.smallVal?.toInt() ?: 0
        var res: LeanObject = LeanCtor(0)
        for (i in 0 until count) {
            res = LeanCtor(1, arrayOf(elem, res))
        }
        return res
    }

    @JvmStatic
    public fun f_List_appendTR___redArg(asObj: LeanObject?, bsObj: LeanObject?): LeanObject {
        val elems = mutableListOf<LeanObject?>()
        var curr = asObj
        while (curr != null && curr.tag != 0) {
            val ctor = curr as? LeanCtor ?: break
            elems.add(ctor.getObj(0))
            curr = ctor.getObj(1)
        }
        var res: LeanObject = bsObj ?: LeanCtor(0)
        for (i in elems.indices.reversed()) {
            res = LeanCtor(1, arrayOf(elems[i], res))
        }
        return res
    }

    @JvmStatic
    public fun f_List_reverse___redArg(asObj: LeanObject?): LeanObject {
        val elems = mutableListOf<LeanObject?>()
        var curr = asObj
        while (curr != null && curr.tag != 0) {
            val ctor = curr as? LeanCtor ?: break
            elems.add(ctor.getObj(0))
            curr = ctor.getObj(1)
        }
        var res: LeanObject = LeanCtor(0)
        for (i in elems.indices) {
            res = LeanCtor(1, arrayOf(elems[i], res))
        }
        return res
    }

    @JvmStatic
    public fun f_List_reverse(type: LeanObject?, asObj: LeanObject?): LeanObject =
        f_List_reverse___redArg(asObj)
}

public object mod_l_Init_Core {
    @JvmStatic
    public fun f_Thunk_mk(type: LeanObject?, closure: LeanObject?): LeanObject {
        return LeanThunk.alloc(closure as? LeanClosure)
    }

    @JvmStatic
    public fun f_Thunk_get(type: LeanObject?, thunk: LeanObject?): LeanObject? {
        return (thunk as? LeanThunk)?.get()
    }

    @JvmStatic
    public fun f_Thunk_pure(type: LeanObject?, value: LeanObject?): LeanObject {
        return LeanThunk.pure(value)
    }
}

public object mod_l_Init_Data_ByteArray_Basic {
    @JvmStatic
    public fun f_ByteArray_mkEmpty(capacity: LeanObject?): LeanObject = LeanByteArray.empty()

    @JvmStatic
    public fun f_ByteArray_push(a: LeanObject?, b: LeanObject?): LeanObject {
        val ba = a as? LeanByteArray ?: LeanByteArray.empty()
        val byteVal = ((b as? LeanNat)?.smallVal ?: 0L).toByte()
        return ba.push(byteVal)
    }

    @JvmStatic
    public fun f_ByteArray_size(a: LeanObject?): LeanObject {
        val s = (a as? LeanByteArray)?.size() ?: 0
        return LeanNat.ofLong(s.toLong())
    }

    @JvmStatic
    public fun f_ByteArray_get(a: LeanObject?, i: LeanObject?): LeanObject {
        val ba = a as? LeanByteArray ?: return LeanNat.ZERO
        val idx = (i as? LeanNat)?.smallVal?.toInt() ?: 0
        return LeanNat.ofLong((ba.get(idx).toInt() and 0xFF).toLong())
    }

    @JvmStatic
    public fun f_ByteArray_get(a: LeanObject?, i: LeanObject?, h: LeanObject?): LeanObject = f_ByteArray_get(a, i)

    @JvmStatic
    public fun f_ByteArray_getInternal(a: LeanObject?, i: LeanObject?): LeanObject = f_ByteArray_get(a, i)

    @JvmStatic
    public fun f_ByteArray_uget(a: LeanObject?, i: LeanObject?): LeanObject = f_ByteArray_get(a, i)

    @JvmStatic
    public fun f_ByteArray_fget(a: LeanObject?, i: LeanObject?): LeanObject = f_ByteArray_get(a, i)
}

public object mod_l_Init_Data_String_Defs {
    @JvmStatic
    public fun f_String_append(a: LeanObject?, b: LeanObject?): LeanObject {
        val aStr = if (a is LeanString) a else LeanString.of(a?.toString() ?: "")
        val bStr = if (b is LeanString) b else LeanString.of(b?.toString() ?: "")
        return LeanString.concat(aStr, bStr)
    }

    @JvmStatic
    public fun f_String_toUTF8(s: LeanObject?): LeanObject {
        val bytes = (s as? LeanString)?.bytes ?: ByteArray(0)
        return LeanByteArray(bytes.copyOf())
    }

    @JvmStatic
    public fun f_String_toByteArray(s: LeanObject?): LeanObject = f_String_toUTF8(s)

    @JvmStatic
    public fun f_String_utf8ByteSize(s: LeanObject?): LeanObject {
        val size = (s as? LeanString)?.byteSize ?: 0
        return LeanNat.ofLong(size.toLong())
    }

    @JvmStatic
    public fun f_String_length(s: LeanObject?): LeanObject {
        val bytes = (s as? LeanString)?.bytes ?: return LeanNat.ZERO
        var count = 0
        for (b in bytes) {
            if ((b.toInt() and 0xC0) != 0x80) {
                count++
            }
        }
        return LeanNat.ofLong(count.toLong())
    }
}

public object mod_l_Init_Data_Int_Repr {
    @JvmStatic
    public fun f_Int_repr(n: LeanObject?): LeanObject {
        val s = if (n is LeanCtor) {
            if (n.tag == 0) {
                val natVal = n.getObj(0)
                natVal?.toString() ?: "0"
            } else {
                val natVal = n.getObj(0)
                val succ = (natVal as? LeanNat)?.add(LeanNat.ONE) ?: LeanNat.ONE
                "-$succ"
            }
        } else if (n is LeanNat) {
            n.toString()
        } else "0"
        return LeanString.of(s)
    }
}

public object mod_l_Init_Data_OfScientific {
    @JvmStatic
    public fun f_Float_ofScientific(m: LeanObject?, s: LeanObject?, e: LeanObject?): LeanObject {
        val mVal = if (m is LeanNat) (if (m.bigVal != null) lean.runtime.bigIntToDouble(m.bigVal) else m.smallVal.toDouble()) else 0.0
        val eVal = (e as? LeanNat)?.smallVal?.toInt() ?: 0
        val isNegExp = (s is LeanNat && s.smallVal != 0L) || (s is LeanCtor && s.tag == 1)
        val power = 10.0.pow(eVal.toDouble())
        val res = if (isNegExp) mVal / power else mVal * power
        return LeanFloat.ofDouble(res)
    }

    @JvmStatic
    public fun f_Float_ofNat(n: LeanObject?): LeanObject {
        val d = if (n is LeanNat) {
            if (n.bigVal != null) lean.runtime.bigIntToDouble(n.bigVal) else n.smallVal.toDouble()
        } else 0.0
        return LeanFloat.ofDouble(d)
    }

    @JvmStatic
    public fun f_Float_ofInt(n: LeanObject?): LeanObject {
        if (n is LeanCtor) {
            if (n.tag == 0) {
                return f_Float_ofNat(n.getObj(0))
            } else {
                val natVal = n.getObj(0)
                val succ = (natVal as? LeanNat)?.add(LeanNat.ONE) ?: LeanNat.ONE
                val d = if (succ.bigVal != null) lean.runtime.bigIntToDouble(succ.bigVal) else succ.smallVal.toDouble()
                return LeanFloat.ofDouble(-d)
            }
        }
        return f_Float_ofNat(n)
    }
}

public object mod_l_Init_Data_Float_Float {
    @JvmStatic
    public fun f_Float_nan(): LeanObject = LeanFloat.NAN

    @JvmStatic
    public fun f_Float_inf(): LeanObject = LeanFloat.POS_INF

    @JvmStatic
    public fun f_Float_add(a: LeanObject?, b: LeanObject?): LeanObject =
        LeanFloat.ofDouble(LeanFloat.toDouble(a) + LeanFloat.toDouble(b))

    @JvmStatic
    public fun f_Float_sub(a: LeanObject?, b: LeanObject?): LeanObject =
        LeanFloat.ofDouble(LeanFloat.toDouble(a) - LeanFloat.toDouble(b))

    @JvmStatic
    public fun f_Float_mul(a: LeanObject?, b: LeanObject?): LeanObject =
        LeanFloat.ofDouble(LeanFloat.toDouble(a) * LeanFloat.toDouble(b))

    @JvmStatic
    public fun f_Float_div(a: LeanObject?, b: LeanObject?): LeanObject =
        LeanFloat.ofDouble(LeanFloat.toDouble(a) / LeanFloat.toDouble(b))

    @JvmStatic
    public fun f_Float_pow(a: LeanObject?, b: LeanObject?): LeanObject =
        LeanFloat.ofDouble(LeanFloat.toDouble(a).pow(LeanFloat.toDouble(b)))

    @JvmStatic
    public fun f_Float_neg(a: LeanObject?): LeanObject =
        LeanFloat.ofDouble(-LeanFloat.toDouble(a))

    @JvmStatic
    public fun f_Float_abs(a: LeanObject?): LeanObject =
        LeanFloat.ofDouble(abs(LeanFloat.toDouble(a)))

    @JvmStatic
    public fun f_Float_decLt(a: LeanObject?, b: LeanObject?): LeanObject =
        if (LeanFloat.toDouble(a) < LeanFloat.toDouble(b)) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_Float_decLe(a: LeanObject?, b: LeanObject?): LeanObject =
        if (LeanFloat.toDouble(a) <= LeanFloat.toDouble(b)) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_Float_decEq(a: LeanObject?, b: LeanObject?): LeanObject =
        if (LeanFloat.toDouble(a) == LeanFloat.toDouble(b)) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_Float_beq(a: LeanObject?, b: LeanObject?): LeanObject =
        if (LeanFloat.toDouble(a) == LeanFloat.toDouble(b)) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_Float_toBits(a: LeanObject?): LeanObject =
        LeanNat.ofLong(LeanFloat.toDouble(a).toRawBits())

    @JvmStatic
    public fun f_Float_toModel(a: LeanObject?): LeanObject = f_Float_toBits(a)

    @JvmStatic
    public fun f_Float_ofBits(bits: LeanObject?): LeanObject {
        val raw = (bits as? LeanNat)?.smallVal ?: 0L
        return LeanFloat.ofDouble(Double.fromBits(raw))
    }

    @JvmStatic
    public fun f_Float_ofModel(bits: LeanObject?): LeanObject = f_Float_ofBits(bits)

    @JvmStatic
    public fun f_UInt64_toFloat(n: LeanObject?): LeanObject {
        val u = if (n is LeanNat) n.smallVal.toULong().toDouble() else 0.0
        return LeanFloat.ofDouble(u)
    }

    @JvmStatic
    public fun f_Float_toUInt8(a: LeanObject?): LeanObject {
        val d = LeanFloat.toDouble(a)
        val v = if (d.isNaN() || d < 0.0) 0L else if (d >= 256.0) 255L else (d.toLong() and 0xFFL)
        return LeanNat.ofLong(v)
    }

    @JvmStatic
    public fun f_Float_toUInt16(a: LeanObject?): LeanObject {
        val d = LeanFloat.toDouble(a)
        val v = if (d.isNaN() || d < 0.0) 0L else if (d >= 65536.0) 65535L else (d.toLong() and 0xFFFFL)
        return LeanNat.ofLong(v)
    }

    @JvmStatic
    public fun f_Float_toUInt32(a: LeanObject?): LeanObject {
        val d = LeanFloat.toDouble(a)
        val v = if (d.isNaN() || d < 0.0) 0L else if (d >= 4294967296.0) 4294967295L else (d.toLong() and 0xFFFFFFFFL)
        return LeanNat.ofLong(v)
    }

    @JvmStatic
    public fun f_Float_toUInt64(a: LeanObject?): LeanObject {
        val d = LeanFloat.toDouble(a)
        val v = if (d.isNaN() || d < 0.0) 0L else if (d >= 18446744073709551616.0) -1L else d.toULong().toLong()
        return LeanNat.ofLong(v)
    }

    @JvmStatic
    public fun f_Float_toUSize(a: LeanObject?): LeanObject = f_Float_toUInt64(a)

    @JvmStatic
    public fun f_Float_isNaN(a: LeanObject?): LeanObject =
        if (LeanFloat.toDouble(a).isNaN()) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_Float_isInf(a: LeanObject?): LeanObject =
        if (LeanFloat.toDouble(a).isInfinite()) LeanNat.ONE else LeanNat.ZERO

    @JvmStatic
    public fun f_Float_isFinite(a: LeanObject?): LeanObject {
        val d = LeanFloat.toDouble(a)
        return if (!d.isNaN() && !d.isInfinite()) LeanNat.ONE else LeanNat.ZERO
    }

    @JvmStatic
    public fun f_Float_toString(a: LeanObject?): LeanObject =
        LeanString.of(LeanFloat.format(LeanFloat.toDouble(a)))

    @JvmStatic
    public fun f_Float_frExp(a: LeanObject?): LeanObject {
        val d = LeanFloat.toDouble(a)
        val pair = LeanCtor.alloc(0, 2, 0)
        if (d == 0.0 || d.isNaN() || d.isInfinite()) {
            pair.setObj(0, LeanFloat.ofDouble(d))
            val intZero = LeanCtor.alloc(0, 1, 0)
            intZero.setObj(0, LeanNat.ZERO)
            pair.setObj(1, intZero)
            return pair
        }
        var exp = floor(log2(abs(d))).toInt() + 1
        var mantissa = d / 2.0.pow(exp.toDouble())
        while (abs(mantissa) >= 1.0) {
            mantissa /= 2.0
            exp++
        }
        while (abs(mantissa) < 0.5) {
            mantissa *= 2.0
            exp--
        }
        pair.setObj(0, LeanFloat.ofDouble(mantissa))
        val intObj = if (exp >= 0) {
            val intCtor = LeanCtor.alloc(0, 1, 0)
            intCtor.setObj(0, LeanNat.ofLong(exp.toLong()))
            intCtor
        } else {
            val intCtor = LeanCtor.alloc(1, 1, 0)
            intCtor.setObj(0, LeanNat.ofLong((-exp - 1).toLong()))
            intCtor
        }
        pair.setObj(1, intObj)
        return pair
    }
}

public object mod_l_Init_System_ST {
    @JvmStatic
    public fun f_ST_Prim_mkRef(sigma: LeanObject?, alpha: LeanObject?, a: LeanObject?, world: LeanObject?): LeanObject =
        LeanRef(a)

    @JvmStatic
    public fun f_ST_Prim_Ref_get(sigma: LeanObject?, alpha: LeanObject?, ref: LeanObject?, world: LeanObject?): LeanObject? =
        (ref as? LeanRef)?.value

    @JvmStatic
    public fun f_ST_Prim_Ref_take(sigma: LeanObject?, alpha: LeanObject?, ref: LeanObject?, world: LeanObject?): LeanObject? =
        (ref as? LeanRef)?.value

    @JvmStatic
    public fun f_ST_Prim_Ref_set(sigma: LeanObject?, alpha: LeanObject?, ref: LeanObject?, a: LeanObject?, world: LeanObject?): LeanObject {
        (ref as? LeanRef)?.value = a
        return LeanNat.ZERO
    }

    @JvmStatic
    public fun f_ST_Prim_Ref_put(sigma: LeanObject?, alpha: LeanObject?, ref: LeanObject?, a: LeanObject?, world: LeanObject?): LeanObject {
        (ref as? LeanRef)?.value = a
        return LeanNat.ZERO
    }

    @JvmStatic
    public fun f_ST_Prim_Ref_swap(sigma: LeanObject?, alpha: LeanObject?, ref: LeanObject?, a: LeanObject?, world: LeanObject?): LeanObject? {
        val r = ref as? LeanRef
        val old = r?.value
        r?.value = a
        return old
    }

    @JvmStatic
    public fun f_ST_Prim_Ref_ptrEq(sigma: LeanObject?, alpha: LeanObject?, r1: LeanObject?, r2: LeanObject?, world: LeanObject?): LeanObject =
        if (r1 === r2) LeanNat.ONE else LeanNat.ZERO
}

public object mod_l_Init_System_IO {
    @JvmStatic
    public fun f_stdout_flush(world: LeanObject?): LeanObject {
        val res = LeanCtor.alloc(0, 2, 0)
        res.setObj(0, LeanNat.ZERO)
        res.setObj(1, world ?: LeanNat.ZERO)
        return res
    }

    @JvmStatic
    public fun f_stdout_putStr(s: LeanObject?, world: LeanObject?): LeanObject {
        print(s?.toString() ?: "")
        val res = LeanCtor.alloc(0, 2, 0)
        res.setObj(0, LeanNat.ZERO)
        res.setObj(1, world ?: LeanNat.ZERO)
        return res
    }

    @JvmStatic
    public fun f_stdout_getLine(world: LeanObject?): LeanObject {
        val line = readLine() ?: ""
        val res = LeanCtor.alloc(0, 2, 0)
        res.setObj(0, LeanString.of(line))
        res.setObj(1, world ?: LeanNat.ZERO)
        return res
    }

    @JvmStatic
    public fun f_stdout_isTty(world: LeanObject?): LeanObject {
        val res = LeanCtor.alloc(0, 2, 0)
        res.setObj(0, LeanNat.ZERO)
        res.setObj(1, world ?: LeanNat.ZERO)
        return res
    }

    private val stdoutStream: LeanCtor by lazy {
        val stream = LeanCtor.alloc(0, 6, 0)
        stream.setObj(0, LeanClosure.alloc("lean/mod_l_Init_System_IO", "f_stdout_flush", 1, emptyArray()))
        stream.setObj(1, LeanNat.ZERO)
        stream.setObj(2, LeanNat.ZERO)
        stream.setObj(3, LeanClosure.alloc("lean/mod_l_Init_System_IO", "f_stdout_getLine", 1, emptyArray()))
        stream.setObj(4, LeanClosure.alloc("lean/mod_l_Init_System_IO", "f_stdout_putStr", 2, emptyArray()))
        stream.setObj(5, LeanClosure.alloc("lean/mod_l_Init_System_IO", "f_stdout_isTty", 1, emptyArray()))
        stream
    }

    @JvmStatic
    public fun f_IO_getStdout(world: LeanObject?): LeanObject = stdoutStream

    @JvmStatic
    public fun f_IO_getStderr(world: LeanObject?): LeanObject = stdoutStream

    @JvmStatic
    public fun f_IO_getStdin(world: LeanObject?): LeanObject = stdoutStream
}



