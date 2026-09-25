/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean;

import lean.runtime.LeanNat;
import lean.runtime.LeanObject;

public final class mod_l_Init_Prelude {
    public static LeanObject f_Nat_add(LeanObject a, LeanObject b) {
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : a != null ? a.getTag() : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : b != null ? b.getTag() : 0L;
        return LeanNat.ofLong(aVal + bVal);
    }

    public static LeanObject f_Nat_sub(LeanObject a, LeanObject b) {
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : a != null ? a.getTag() : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : b != null ? b.getTag() : 0L;
        return LeanNat.ofLong(aVal < bVal ? 0L : aVal - bVal);
    }

    public static LeanObject f_Nat_mul(LeanObject a, LeanObject b) {
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : a != null ? a.getTag() : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : b != null ? b.getTag() : 0L;
        return LeanNat.ofLong(aVal * bVal);
    }

    public static LeanObject f_Nat_div(LeanObject a, LeanObject b) {
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : a != null ? a.getTag() : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : b != null ? b.getTag() : 0L;
        if (bVal == 0L) return LeanNat.ZERO;
        return LeanNat.ofLong(aVal / bVal);
    }

    public static LeanObject f_Nat_mod(LeanObject a, LeanObject b) {
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : a != null ? a.getTag() : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : b != null ? b.getTag() : 0L;
        if (bVal == 0L) return LeanNat.ZERO;
        return LeanNat.ofLong(aVal % bVal);
    }

    public static LeanObject f_Nat_decEq(LeanObject a, LeanObject b) {
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : a != null ? a.getTag() : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : b != null ? b.getTag() : 0L;
        return aVal == bVal ? LeanNat.ONE : LeanNat.ZERO;
    }

    public static LeanObject f_Nat_decLe(LeanObject a, LeanObject b) {
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : a != null ? a.getTag() : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : b != null ? b.getTag() : 0L;
        return aVal <= bVal ? LeanNat.ONE : LeanNat.ZERO;
    }

    public static LeanObject f_Nat_decLt(LeanObject a, LeanObject b) {
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : a != null ? a.getTag() : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : b != null ? b.getTag() : 0L;
        return aVal < bVal ? LeanNat.ONE : LeanNat.ZERO;
    }

    public static LeanObject f_Nat_ble(LeanObject a, LeanObject b) {
        return f_Nat_decLe(a, b);
    }

    public static LeanObject f_Nat_blt(LeanObject a, LeanObject b) {
        return f_Nat_decLt(a, b);
    }

    public static LeanObject f_UInt64_decEq(LeanObject a, LeanObject b) {
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : a != null ? a.getTag() : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : b != null ? b.getTag() : 0L;
        return aVal == bVal ? LeanNat.ONE : LeanNat.ZERO;
    }

    public static LeanObject f_UInt32_decEq(LeanObject a, LeanObject b) {
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : a != null ? a.getTag() : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : b != null ? b.getTag() : 0L;
        return (aVal & 0xFFFFFFFFL) == (bVal & 0xFFFFFFFFL) ? LeanNat.ONE : LeanNat.ZERO;
    }

    public static LeanObject f_UInt8_decEq(LeanObject a, LeanObject b) {
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : a != null ? a.getTag() : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : b != null ? b.getTag() : 0L;
        return (aVal & 0xFFL) == (bVal & 0xFFL) ? LeanNat.ONE : LeanNat.ZERO;
    }

    public static LeanObject f_USize_decEq(LeanObject a, LeanObject b) {
        return f_UInt64_decEq(a, b);
    }
}
