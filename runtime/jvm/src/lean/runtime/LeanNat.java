/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime;

import java.math.BigInteger;

public final class LeanNat extends LeanObject {
    public final long smallVal;
    public final BigInteger bigVal;

    public static final LeanNat ZERO = new LeanNat(0L);
    public static final LeanNat ONE = new LeanNat(1L);

    public LeanNat(long smallVal) {
        this.smallVal = smallVal;
        this.bigVal = null;
    }

    public LeanNat(BigInteger bigVal) {
        if (bigVal != null && bigVal.compareTo(BigInteger.valueOf(Long.MAX_VALUE)) <= 0 && bigVal.compareTo(BigInteger.ZERO) >= 0) {
            this.smallVal = bigVal.longValue();
            this.bigVal = null;
        } else {
            this.smallVal = -1L;
            this.bigVal = bigVal != null ? bigVal : BigInteger.ZERO;
        }
    }

    public BigInteger toBigInteger() {
        if (bigVal != null) return bigVal;
        return BigInteger.valueOf(smallVal);
    }

    @Override
    public int getTag() {
        return (int) (bigVal != null ? bigVal.intValue() : smallVal);
    }

    @Override
    public String toString() {
        return bigVal != null ? bigVal.toString() : Long.toString(smallVal);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LeanNat)) return false;
        LeanNat other = (LeanNat) o;
        if (bigVal != null || other.bigVal != null) {
            return toBigInteger().equals(other.toBigInteger());
        }
        return smallVal == other.smallVal;
    }

    @Override
    public int hashCode() {
        return bigVal != null ? bigVal.hashCode() : Long.hashCode(smallVal);
    }

    public static LeanNat ofLong(long v) {
        if (v == 0L) return ZERO;
        if (v == 1L) return ONE;
        return new LeanNat(v);
    }

    public static LeanNat ofBigInteger(BigInteger b) {
        if (b == null || b.equals(BigInteger.ZERO)) return ZERO;
        if (b.equals(BigInteger.ONE)) return ONE;
        return new LeanNat(b);
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
