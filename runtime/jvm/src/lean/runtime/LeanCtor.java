/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime;

public final class LeanCtor extends LeanObject {
    public final int tag;
    public final LeanObject[] objs;
    public final long[] scalars;

    private static final LeanObject[] EMPTY_OBJS = new LeanObject[0];
    private static final long[] EMPTY_SCALARS = new long[0];

    public LeanCtor(int tag, LeanObject[] objs, long[] scalars) {
        this.tag = tag;
        this.objs = objs != null ? objs : EMPTY_OBJS;
        this.scalars = scalars != null ? scalars : EMPTY_SCALARS;
    }

    @Override
    public int getTag() {
        return tag;
    }

    public LeanObject getObj(int index) {
        return objs[index];
    }

    public void setObj(int index, LeanObject val) {
        objs[index] = val;
    }

    public long getScalar(int offset) {
        return scalars[offset];
    }

    public void setScalar(int offset, long val) {
        scalars[offset] = val;
    }

    public static LeanCtor alloc(int tag, int numObjs, int numScalars) {
        LeanObject[] objs = numObjs == 0 ? EMPTY_OBJS : new LeanObject[numObjs];
        long[] scalars = numScalars == 0 ? EMPTY_SCALARS : new long[numScalars];
        return new LeanCtor(tag, objs, scalars);
    }
}
