/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean;

import lean.runtime.LeanNat;
import lean.runtime.LeanObject;

public final class mod_l_Init_Data_UInt_BasicAux {
    public static LeanObject f_UInt64_ofNat(LeanObject a) {
        return a != null ? a : LeanNat.ZERO;
    }

    public static LeanObject f_UInt64_toNat(LeanObject a) {
        return a != null ? a : LeanNat.ZERO;
    }

    public static LeanObject f_UInt32_ofNat(LeanObject a) {
        if (a instanceof LeanNat) {
            return LeanNat.ofLong(((LeanNat) a).smallVal & 0xFFFFFFFFL);
        }
        return a != null ? a : LeanNat.ZERO;
    }

    public static LeanObject f_UInt32_toNat(LeanObject a) {
        return a != null ? a : LeanNat.ZERO;
    }

    public static LeanObject f_UInt8_ofNat(LeanObject a) {
        if (a instanceof LeanNat) {
            return LeanNat.ofLong(((LeanNat) a).smallVal & 0xFFL);
        }
        return a != null ? a : LeanNat.ZERO;
    }

    public static LeanObject f_UInt8_toNat(LeanObject a) {
        return a != null ? a : LeanNat.ZERO;
    }

    public static LeanObject f_USize_ofNat(LeanObject a) {
        return a != null ? a : LeanNat.ZERO;
    }

    public static LeanObject f_USize_toNat(LeanObject a) {
        return a != null ? a : LeanNat.ZERO;
    }
}
