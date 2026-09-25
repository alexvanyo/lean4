/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean;

import lean.runtime.LeanCtor;
import lean.runtime.LeanNat;
import lean.runtime.LeanObject;

public final class mod_l_Init_Data_Option_Basic {
    public static LeanObject f_Option_instDecidableEq___redArg(LeanObject eqDec, LeanObject a, LeanObject b) {
        if (a == null && b == null) return LeanNat.ONE;
        if (a == null || b == null) return LeanNat.ZERO;
        if (!(a instanceof LeanCtor) || !(b instanceof LeanCtor)) return LeanNat.ZERO;
        LeanCtor aCtor = (LeanCtor) a;
        LeanCtor bCtor = (LeanCtor) b;
        if (aCtor.tag != bCtor.tag) return LeanNat.ZERO;
        if (aCtor.tag == 0) return LeanNat.ONE; // none == none
        LeanObject aVal = aCtor.getObj(0);
        LeanObject bVal = bCtor.getObj(0);
        if (aVal == null && bVal == null) return LeanNat.ONE;
        if (aVal == null || bVal == null) return LeanNat.ZERO;
        if (aVal.equals(bVal)) return LeanNat.ONE;
        if (aVal instanceof LeanNat && bVal instanceof LeanNat) {
            return ((LeanNat) aVal).smallVal == ((LeanNat) bVal).smallVal ? LeanNat.ONE : LeanNat.ZERO;
        }
        return LeanNat.ZERO;
    }

    public static LeanObject f_Option_isSome(LeanObject a) {
        if (a instanceof LeanCtor && ((LeanCtor) a).tag == 1) return LeanNat.ONE;
        return LeanNat.ZERO;
    }

    public static LeanObject f_Option_isNone(LeanObject a) {
        if (a instanceof LeanCtor && ((LeanCtor) a).tag == 0) return LeanNat.ONE;
        return LeanNat.ZERO;
    }

    public static LeanObject f_Option_getD(LeanObject a, LeanObject fallback) {
        if (a instanceof LeanCtor && ((LeanCtor) a).tag == 1) {
            return ((LeanCtor) a).getObj(0);
        }
        return fallback;
    }
}
