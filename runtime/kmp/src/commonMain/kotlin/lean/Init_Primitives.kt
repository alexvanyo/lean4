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

import lean.runtime.LeanCtor
import lean.runtime.LeanNat
import lean.runtime.LeanObject
import lean.runtime.LeanString
import kotlin.jvm.JvmStatic

/**
 * Kotlin Multiplatform runtime primitives for Lean's standard `Init` library modules.
 */
public object mod_l_Init_Prelude {
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
        val aVal = if (a is LeanNat) a.smallVal else (a?.tag?.toLong() ?: 0L)
        val bVal = if (b is LeanNat) b.smallVal else (b?.tag?.toLong() ?: 0L)
        if (bVal >= 64L) return LeanNat.ZERO
        return LeanNat.ofLong(aVal ushr bVal.toInt())
    }

    @JvmStatic
    public fun f_Nat_shiftLeft(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else (a?.tag?.toLong() ?: 0L)
        val bVal = if (b is LeanNat) b.smallVal else (b?.tag?.toLong() ?: 0L)
        if (bVal >= 64L) return LeanNat.ZERO
        return LeanNat.ofLong(aVal shl bVal.toInt())
    }

    @JvmStatic
    public fun f_Nat_land(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else 0L
        val bVal = if (b is LeanNat) b.smallVal else 0L
        return LeanNat.ofLong(aVal and bVal)
    }

    @JvmStatic
    public fun f_Nat_lor(a: LeanObject?, b: LeanObject?): LeanObject {
        val aVal = if (a is LeanNat) a.smallVal else 0L
        val bVal = if (b is LeanNat) b.smallVal else 0L
        return LeanNat.ofLong(aVal or bVal)
    }

    @JvmStatic
    public fun f_Nat_xor(a: LeanObject?, b: LeanObject?): LeanObject {
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
