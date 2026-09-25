/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime;

public final class LeanArray extends LeanObject {
    public LeanObject[] data;

    public LeanArray(LeanObject[] data) {
        this.data = data != null ? data : new LeanObject[0];
    }

    public int size() {
        return data.length;
    }

    public LeanObject get(int index) {
        return data[index];
    }

    public void set(int index, LeanObject val) {
        data[index] = val;
    }

    public LeanArray push(LeanObject val) {
        LeanObject[] newArr = new LeanObject[data.length + 1];
        System.arraycopy(data, 0, newArr, 0, data.length);
        newArr[data.length] = val;
        return new LeanArray(newArr);
    }

    public static LeanArray empty() {
        return new LeanArray(new LeanObject[0]);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("#[");
        for (int i = 0; i < data.length; i++) {
            if (i > 0) sb.append(", ");
            sb.append(data[i]);
        }
        sb.append("]");
        return sb.toString();
    }
}
