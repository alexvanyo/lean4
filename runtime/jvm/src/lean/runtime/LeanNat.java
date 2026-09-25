/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime;

public final class LeanNat extends LeanObject {
    public final long smallVal;

    public static final LeanNat ZERO = new LeanNat(0L);
    public static final LeanNat ONE = new LeanNat(1L);

    public LeanNat(long smallVal) {
        this.smallVal = smallVal;
    }

    @Override
    public int getTag() {
        return (int) smallVal;
    }

    @Override
    public String toString() {
        return Long.toString(smallVal);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LeanNat)) return false;
        return smallVal == ((LeanNat) o).smallVal;
    }

    @Override
    public int hashCode() {
        return Long.hashCode(smallVal);
    }

    public static LeanNat ofLong(long v) {
        if (v == 0L) return ZERO;
        if (v == 1L) return ONE;
        return new LeanNat(v);
    }

    public static LeanNat add(LeanNat a, LeanNat b) {
        return new LeanNat(a.smallVal + b.smallVal);
    }

    public static LeanNat sub(LeanNat a, LeanNat b) {
        return new LeanNat(a.smallVal < b.smallVal ? 0L : a.smallVal - b.smallVal);
    }

    public static LeanNat mul(LeanNat a, LeanNat b) {
        return new LeanNat(a.smallVal * b.smallVal);
    }

    public static LeanNat div(LeanNat a, LeanNat b) {
        if (b.smallVal == 0L) return ZERO;
        return new LeanNat(a.smallVal / b.smallVal);
    }

    public static LeanNat mod(LeanNat a, LeanNat b) {
        if (b.smallVal == 0L) return ZERO;
        return new LeanNat(a.smallVal % b.smallVal);
    }

    public static boolean ble(LeanNat a, LeanNat b) {
        return a.smallVal <= b.smallVal;
    }

    public static boolean blt(LeanNat a, LeanNat b) {
        return a.smallVal < b.smallVal;
    }
}
