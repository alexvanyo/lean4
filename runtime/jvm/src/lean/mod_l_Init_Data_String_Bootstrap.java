/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean;

import lean.runtime.LeanNat;
import lean.runtime.LeanObject;
import lean.runtime.LeanString;

public final class mod_l_Init_Data_String_Bootstrap {
    public static LeanObject f_String_Internal_length(LeanObject s) {
        long len = s instanceof LeanString ? ((LeanString) s).getByteSize() : 0L;
        return LeanNat.ofLong(len);
    }

    public static LeanObject f_String_push(LeanObject str, LeanObject c) {
        String base = str instanceof LeanString ? str.toString() : "";
        long codePoint = c instanceof LeanNat ? ((LeanNat) c).smallVal : c != null ? c.getTag() : 0L;
        char[] chars = Character.toChars((int) codePoint);
        return LeanString.of(base + new String(chars));
    }

    public static LeanObject f_String_append(LeanObject a, LeanObject b) {
        if (a instanceof LeanString && b instanceof LeanString) {
            return LeanString.concat((LeanString) a, (LeanString) b);
        }
        String s1 = a != null ? a.toString() : "";
        String s2 = b != null ? b.toString() : "";
        return LeanString.of(s1 + s2);
    }

    public static LeanObject f_String_isEmpty(LeanObject s) {
        if (s instanceof LeanString) {
            return ((LeanString) s).getByteSize() == 0 ? LeanNat.ONE : LeanNat.ZERO;
        }
        return LeanNat.ONE;
    }
}
