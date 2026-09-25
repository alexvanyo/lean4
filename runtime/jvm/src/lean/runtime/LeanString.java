/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;

public final class LeanString extends LeanObject {
    public final byte[] bytes;

    public LeanString(byte[] bytes) {
        this.bytes = bytes != null ? bytes : new byte[0];
    }

    public int getByteSize() {
        return bytes.length;
    }

    public LeanString substringByByte(int startByte, int endByte) {
        return new LeanString(Arrays.copyOfRange(bytes, startByte, endByte));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LeanString)) return false;
        return Arrays.equals(bytes, ((LeanString) o).bytes);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(bytes);
    }

    @Override
    public String toString() {
        return new String(bytes, StandardCharsets.UTF_8);
    }

    public static LeanString of(String str) {
        return new LeanString(str.getBytes(StandardCharsets.UTF_8));
    }

    public static LeanString concat(LeanString a, LeanString b) {
        byte[] res = new byte[a.bytes.length + b.bytes.length];
        System.arraycopy(a.bytes, 0, res, 0, a.bytes.length);
        System.arraycopy(b.bytes, 0, res, a.bytes.length, b.bytes.length);
        return new LeanString(res);
    }
}
