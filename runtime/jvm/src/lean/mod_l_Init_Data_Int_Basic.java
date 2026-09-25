/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean;

import lean.runtime.LeanNat;
import lean.runtime.LeanObject;

public final class mod_l_Init_Data_Int_Basic {
    public static LeanObject f_Int_ofNat(LeanObject n) {
        return n != null ? n : LeanNat.ZERO;
    }

    public static LeanObject f_Int_negOfNat(LeanObject n) {
        long v = n instanceof LeanNat ? ((LeanNat) n).smallVal : 0L;
        return LeanNat.ofLong(-v);
    }

    public static LeanObject f_Int_add(LeanObject a, LeanObject b) {
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : 0L;
        return LeanNat.ofLong(aVal + bVal);
    }

    public static LeanObject f_Int_sub(LeanObject a, LeanObject b) {
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : 0L;
        return LeanNat.ofLong(aVal - bVal);
    }

    public static LeanObject f_Int_mul(LeanObject a, LeanObject b) {
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : 0L;
        return LeanNat.ofLong(aVal * bVal);
    }

    public static LeanObject f_Int_decEq(LeanObject a, LeanObject b) {
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : 0L;
        return aVal == bVal ? LeanNat.ONE : LeanNat.ZERO;
    }

    public static LeanObject f_Int_decLe(LeanObject a, LeanObject b) {
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : 0L;
        return aVal <= bVal ? LeanNat.ONE : LeanNat.ZERO;
    }

    public static LeanObject f_Int_decLt(LeanObject a, LeanObject b) {
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : 0L;
        return aVal < bVal ? LeanNat.ONE : LeanNat.ZERO;
    }
}
