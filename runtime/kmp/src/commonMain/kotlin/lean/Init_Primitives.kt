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
import lean.runtime.LeanClosure
import lean.runtime.LeanCtor
import lean.runtime.LeanNat
import lean.runtime.LeanObject
import lean.runtime.LeanString
import lean.runtime.LeanThunk
import kotlin.jvm.JvmStatic

/**
 * Kotlin Multiplatform runtime primitives for Lean's standard `Init` library modules.
 */
public object mod_l_Init_Prelude {
    @JvmStatic
    public fun f_Array_mkEmpty(type: LeanObject?, capacity: LeanObject?): LeanObject = LeanArray.empty()

    @JvmStatic
    public fun f_Array_emptyWithCapacity(type: LeanObject?, capacity: LeanObject?): LeanObject = LeanArray.empty()

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
    public fun f_Nat_add(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else (a?.tag?.toLong() ?: 0L)
        val bVal = if (b is LeanNat) b.smallVal else (b?.tag?.toLong() ?: 0L)
        return LeanNat.ofLong(aVal + bVal)
    }

    @JvmStatic
    public fun f_Nat_sub(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else (a?.tag?.toLong() ?: 0L)
        val bVal = if (b is LeanNat) b.smallVal else (b?.tag?.toLong() ?: 0L)
        return LeanNat.ofLong(if (aVal < bVal) 0L else aVal - bVal)
    }

    @JvmStatic
    public fun f_Nat_mul(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else (a?.tag?.toLong() ?: 0L)
        val bVal = if (b is LeanNat) b.smallVal else (b?.tag?.toLong() ?: 0L)
        return LeanNat.ofLong(aVal * bVal)
    }

    @JvmStatic
    public fun f_Nat_div(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else (a?.tag?.toLong() ?: 0L)
        val bVal = if (b is LeanNat) b.smallVal else (b?.tag?.toLong() ?: 0L)
        if (bVal == 0L) return LeanNat.ZERO
        return LeanNat.ofLong(aVal / bVal)
    }

    @JvmStatic
    public fun f_Nat_mod(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else (a?.tag?.toLong() ?: 0L)
        val bVal = if (b is LeanNat) b.smallVal else (b?.tag?.toLong() ?: 0L)
        if (bVal == 0L) return LeanNat.ZERO
        return LeanNat.ofLong(aVal % bVal)
    }

    @JvmStatic
    public fun f_UInt64_decEq(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else (a?.tag?.toLong() ?: 0L)
        val bVal = if (b is LeanNat) b.smallVal else (b?.tag?.toLong() ?: 0L)
        return if (aVal == bVal) LeanNat.ONE else LeanNat.ZERO
    }

    @JvmStatic
    public fun f_UInt32_decEq(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else (a?.tag?.toLong() ?: 0L)
        val bVal = if (b is LeanNat) b.smallVal else (b?.tag?.toLong() ?: 0L)
        return if ((aVal and 0xFFFFFFFFL) == (bVal and 0xFFFFFFFFL)) LeanNat.ONE else LeanNat.ZERO
    }

    @JvmStatic
    public fun f_Nat_decLe(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else (a?.tag?.toLong() ?: 0L)
        val bVal = if (b is LeanNat) b.smallVal else (b?.tag?.toLong() ?: 0L)
        return if (aVal <= bVal) LeanNat.ONE else LeanNat.ZERO
    }

    @JvmStatic
    public fun f_Nat_decEq(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else (a?.tag?.toLong() ?: 0L)
        val bVal = if (b is LeanNat) b.smallVal else (b?.tag?.toLong() ?: 0L)
        return if (aVal == bVal) LeanNat.ONE else LeanNat.ZERO
    }

    @JvmStatic
    public fun f_Nat_decLt(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else (a?.tag?.toLong() ?: 0L)
        val bVal = if (b is LeanNat) b.smallVal else (b?.tag?.toLong() ?: 0L)
        return if (aVal < bVal) LeanNat.ONE else LeanNat.ZERO
    }

    @JvmStatic
    public fun f_Nat_ble(a: LeanObject?, b: LeanObject?): LeanObject = f_Nat_decLe(a, b)

    @JvmStatic
    public fun f_Nat_blt(a: LeanObject?, b: LeanObject?): LeanObject = f_Nat_decLt(a, b)
}

public object mod_l_Init_Data_UInt_BasicAux {
    @JvmStatic
    public fun f_UInt64_ofNat(a: LeanObject?): LeanObject = a ?: LeanNat.ZERO

    @JvmStatic
    public fun f_UInt64_toNat(a: LeanObject?): LeanObject = a ?: LeanNat.ZERO

    @JvmStatic
    public fun f_UInt32_ofNat(a: LeanObject?): LeanObject =
        if (a is LeanNat) LeanNat.ofLong(a.smallVal and 0xFFFFFFFFL) else a ?: LeanNat.ZERO

    @JvmStatic
    public fun f_UInt32_toNat(a: LeanObject?): LeanObject = a ?: LeanNat.ZERO

    @JvmStatic
    public fun f_UInt8_ofNat(a: LeanObject?): LeanObject =
        if (a is LeanNat) LeanNat.ofLong(a.smallVal and 0xFFL) else a ?: LeanNat.ZERO

    @JvmStatic
    public fun f_UInt8_toNat(a: LeanObject?): LeanObject = a ?: LeanNat.ZERO

    @JvmStatic
    public fun f_USize_ofNat(a: LeanObject?): LeanObject = a ?: LeanNat.ZERO

    @JvmStatic
    public fun f_USize_toNat(a: LeanObject?): LeanObject = a ?: LeanNat.ZERO
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
    @JvmStatic
    public fun f_Int_ofNat(n: LeanObject?): LeanObject = n ?: LeanNat.ZERO

    @JvmStatic
    public fun f_Int_negOfNat(n: LeanObject?): LeanObject {
        val v = if (n is LeanNat) n.smallVal else 0L
        return LeanNat.ofLong(-v)
    }

    @JvmStatic
    public fun f_Int_add(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else 0L
        val bVal = if (b is LeanNat) b.smallVal else 0L
        return LeanNat.ofLong(aVal + bVal)
    }

    @JvmStatic
    public fun f_Int_sub(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else 0L
        val bVal = if (b is LeanNat) b.smallVal else 0L
        return LeanNat.ofLong(aVal - bVal)
    }

    @JvmStatic
    public fun f_Int_mul(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else 0L
        val bVal = if (b is LeanNat) b.smallVal else 0L
        return LeanNat.ofLong(aVal * bVal)
    }

    @JvmStatic
    public fun f_Int_decEq(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else 0L
        val bVal = if (b is LeanNat) b.smallVal else 0L
        return if (aVal == bVal) LeanNat.ONE else LeanNat.ZERO
    }

    @JvmStatic
    public fun f_Int_decLe(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else 0L
        val bVal = if (b is LeanNat) b.smallVal else 0L
        return if (aVal <= bVal) LeanNat.ONE else LeanNat.ZERO
    }

    @JvmStatic
    public fun f_Int_decLt(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else 0L
        val bVal = if (b is LeanNat) b.smallVal else 0L
        return if (aVal < bVal) LeanNat.ONE else LeanNat.ZERO
    }
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
        val isTrue = if (b is LeanNat) b.smallVal != 0L else ((b?.tag ?: 0) != 0)
        return LeanString.of(if (isTrue) "true" else "false")
    }

    @JvmStatic
    public fun f_Nat_reprFast(n: LeanObject?): LeanObject {
        val v = if (n is LeanNat) n.smallVal else 0L
        return LeanString.of(v.toString())
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



