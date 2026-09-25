/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime;

public final class LeanThunk extends LeanObject {
    private LeanObject value;
    private LeanClosure closure;

    public LeanThunk(LeanClosure closure) {
        this.closure = closure;
        this.value = null;
    }

    public LeanThunk(LeanObject value) {
        this.closure = null;
        this.value = value;
    }

    public synchronized LeanObject get() {
        if (value == null && closure != null) {
            value = closure.apply();
            closure = null;
        }
        return value;
    }

    public static LeanThunk alloc(LeanClosure closure) {
        return new LeanThunk(closure);
    }

    public static LeanThunk pure(LeanObject value) {
        return new LeanThunk(value);
    }
}
