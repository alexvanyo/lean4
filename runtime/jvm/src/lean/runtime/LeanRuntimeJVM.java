/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime;

public final class LeanRuntimeJVM {

    public static LeanObject boxUInt8(byte v) {
        return LeanNat.ofLong(v & 0xFF);
    }

    public static LeanObject boxUInt16(short v) {
        return LeanNat.ofLong(v & 0xFFFF);
    }

    public static LeanObject boxUInt32(int v) {
        return LeanNat.ofLong(v & 0xFFFFFFFFL);
    }

    public static LeanObject boxUInt64(long v) {
        return LeanNat.ofLong(v);
    }

    public static LeanObject boxUSize(long v) {
        return LeanNat.ofLong(v);
    }

    public static int unboxUInt32(LeanObject obj) {
        if (obj instanceof LeanNat) {
            return (int) ((LeanNat) obj).smallVal;
        }
        return 0;
    }

    public static long unboxUInt64(LeanObject obj) {
        if (obj instanceof LeanNat) {
            return ((LeanNat) obj).smallVal;
        }
        return 0L;
    }

    public static void printString(LeanString str) {
        System.out.print(str.toString());
    }

    public static void printLnString(LeanString str) {
        System.out.println(str.toString());
    }

    public static LeanObject apply1(LeanObject fn, LeanObject a1) {
        return ((LeanClosure) fn).apply1(a1);
    }

    public static LeanObject apply2(LeanObject fn, LeanObject a1, LeanObject a2) {
        return ((LeanClosure) fn).apply2(a1, a2);
    }

    public static LeanObject apply3(LeanObject fn, LeanObject a1, LeanObject a2, LeanObject a3) {
        return ((LeanClosure) fn).apply3(a1, a2, a3);
    }

    public static LeanObject applyN(LeanObject fn, LeanObject... args) {
        return ((LeanClosure) fn).apply(args);
    }
}
