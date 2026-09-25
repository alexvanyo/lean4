/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean;

import lean.runtime.LeanNat;
import lean.runtime.LeanObject;
import lean.runtime.LeanString;

public final class mod_l_Init_Data_Repr {
    public static LeanObject f_Repr_addAppParen(LeanObject a, LeanObject b) {
        return a != null ? a : LeanString.of("");
    }

    public static LeanObject f_Bool_repr___redArg(LeanObject b) {
        boolean isTrue = b instanceof LeanNat ? ((LeanNat) b).smallVal != 0L : b != null && b.getTag() != 0;
        return LeanString.of(isTrue ? "true" : "false");
    }

    public static LeanObject f_Nat_reprFast(LeanObject n) {
        long v = n instanceof LeanNat ? ((LeanNat) n).smallVal : 0L;
        return LeanString.of(Long.toString(v));
    }

    public static LeanObject f_Option_repr___redArg(LeanObject inst, LeanObject opt, LeanObject prec) {
        return LeanString.of(opt != null ? opt.toString() : "none");
    }
}
