/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime;

public final class LeanIOResult extends LeanObject {
    public final boolean isOk;
    public final LeanObject value;
    public final LeanObject error;

    public LeanIOResult(boolean isOk, LeanObject value, LeanObject error) {
        this.isOk = isOk;
        this.value = value;
        this.error = error;
    }

    public static LeanIOResult ok(LeanObject val) {
        return new LeanIOResult(true, val, null);
    }

    public static LeanIOResult error(LeanObject err) {
        return new LeanIOResult(false, null, err);
    }
}
