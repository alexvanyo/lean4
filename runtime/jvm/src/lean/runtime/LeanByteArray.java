/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime;

import java.util.Arrays;

public final class LeanByteArray extends LeanObject {
    public final byte[] data;

    private static final byte[] EMPTY_BYTES = new byte[0];
    public static final LeanByteArray EMPTY = new LeanByteArray(EMPTY_BYTES);

    public LeanByteArray(byte[] data) {
        this.data = data != null ? data : EMPTY_BYTES;
    }

    public int size() {
        return data.length;
    }

    public byte get(int index) {
        return data[index];
    }

    public LeanByteArray push(byte b) {
        byte[] copy = Arrays.copyOf(data, data.length + 1);
        copy[data.length] = b;
        return new LeanByteArray(copy);
    }

    public LeanByteArray set(int index, byte b) {
        byte[] copy = Arrays.copyOf(data, data.length);
        copy[index] = b;
        return new LeanByteArray(copy);
    }

    public static LeanByteArray empty() {
        return EMPTY;
    }

    public static LeanByteArray of(byte[] bytes) {
        return new LeanByteArray(bytes);
    }
}
