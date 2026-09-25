/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

import java.lang.invoke.MethodHandle

/**
 * JVM-specific Lean runtime utilities and MethodHandle-based closures.
 */
public object LeanRuntimeJVM {

    @JvmStatic
    public fun boxUInt8(v: Byte): LeanObject = LeanNat.ofLong((v.toInt() and 0xFF).toLong())

    @JvmStatic
    public fun boxUInt16(v: Short): LeanObject = LeanNat.ofLong((v.toInt() and 0xFFFF).toLong())

    @JvmStatic
    public fun boxUInt32(v: Int): LeanObject = LeanNat.ofLong(v.toLong() and 0xFFFFFFFFL)

    @JvmStatic
    public fun boxUInt64(v: Long): LeanObject = LeanNat.ofLong(v)

    @JvmStatic
    public fun boxUSize(v: Long): LeanObject = LeanNat.ofLong(v)

    @JvmStatic
    public fun unboxUInt32(obj: LeanObject?): Int {
        return when (obj) {
            is LeanNat -> obj.smallVal.toInt()
            else -> 0
        }
    }

    @JvmStatic
    public fun unboxUInt64(obj: LeanObject?): Long {
        return when (obj) {
            is LeanNat -> obj.smallVal
            else -> 0L
        }
    }

    @JvmStatic
    public fun printString(str: LeanString) {
        System.out.print(str.toString())
    }

    @JvmStatic
    public fun printLnString(str: LeanString) {
        System.out.println(str.toString())
    }
}
