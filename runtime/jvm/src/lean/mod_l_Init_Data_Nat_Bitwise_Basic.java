/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean;

import lean.runtime.LeanNat;
import lean.runtime.LeanObject;

public final class mod_l_Init_Data_Nat_Bitwise_Basic {
    public static LeanObject f_Nat_shiftRight(LeanObject a, LeanObject b) {
        if (a instanceof LeanNat && b instanceof LeanNat) {
            LeanNat nA = (LeanNat) a;
            LeanNat nB = (LeanNat) b;
            long bVal = nB.bigVal != null ? nB.bigVal.longValue() : nB.smallVal;
            if (bVal < 0 || bVal > Integer.MAX_VALUE) return LeanNat.ZERO;
            return LeanNat.ofBigInteger(nA.toBigInteger().shiftRight((int) bVal));
        }
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : a != null ? a.getTag() : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : b != null ? b.getTag() : 0L;
        if (bVal >= 64L) return LeanNat.ZERO;
        return LeanNat.ofLong(aVal >>> (int) bVal);
    }

    public static LeanObject f_Nat_shiftLeft(LeanObject a, LeanObject b) {
        if (a instanceof LeanNat && b instanceof LeanNat) {
            LeanNat nA = (LeanNat) a;
            LeanNat nB = (LeanNat) b;
            long bVal = nB.bigVal != null ? nB.bigVal.longValue() : nB.smallVal;
            if (bVal < 0 || bVal > 1_000_000) return LeanNat.ZERO;
            return LeanNat.ofBigInteger(nA.toBigInteger().shiftLeft((int) bVal));
        }
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : a != null ? a.getTag() : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : b != null ? b.getTag() : 0L;
        if (bVal >= 64L) return LeanNat.ZERO;
        return LeanNat.ofLong(aVal << (int) bVal);
    }

    public static LeanObject f_Nat_land(LeanObject a, LeanObject b) {
        if (a instanceof LeanNat && b instanceof LeanNat) {
            return LeanNat.ofBigInteger(((LeanNat) a).toBigInteger().and(((LeanNat) b).toBigInteger()));
        }
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : 0L;
        return LeanNat.ofLong(aVal & bVal);
    }

    public static LeanObject f_Nat_lor(LeanObject a, LeanObject b) {
        if (a instanceof LeanNat && b instanceof LeanNat) {
            return LeanNat.ofBigInteger(((LeanNat) a).toBigInteger().or(((LeanNat) b).toBigInteger()));
        }
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : 0L;
        return LeanNat.ofLong(aVal | bVal);
    }

    public static LeanObject f_Nat_xor(LeanObject a, LeanObject b) {
        if (a instanceof LeanNat && b instanceof LeanNat) {
            return LeanNat.ofBigInteger(((LeanNat) a).toBigInteger().xor(((LeanNat) b).toBigInteger()));
        }
        long aVal = a instanceof LeanNat ? ((LeanNat) a).smallVal : 0L;
        long bVal = b instanceof LeanNat ? ((LeanNat) b).smallVal : 0L;
        return LeanNat.ofLong(aVal ^ bVal);
    }
}
