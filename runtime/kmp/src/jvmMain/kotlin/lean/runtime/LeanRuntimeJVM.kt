/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

import java.lang.invoke.MethodHandle

public actual fun formatDouble6(v: Double): String =
    java.lang.String.format(java.util.Locale.US, "%.6f", v)

/**
 * JVM-specific Lean runtime utilities and MethodHandle-based closures.
 */
public object LeanRuntimeJVM {

    @JvmStatic
    public fun getScalar64(obj: LeanObject?): Long {
        if (obj is LeanFloat) return obj.value.toRawBits()
        if (obj is LeanNat) return obj.smallVal
        if (obj is LeanCtor && obj.scalars.isNotEmpty()) return obj.scalars[0]
        return obj?.tag?.toLong() ?: 0L
    }

    @JvmStatic
    public fun boxUInt8(v: Byte): LeanObject = LeanNat.ofLong((v.toInt() and 0xFF).toLong())

    @JvmStatic
    public fun boxUInt16(v: Short): LeanObject = LeanNat.ofLong((v.toInt() and 0xFFFF).toLong())

    @JvmStatic
    public fun boxUInt32(v: Int): LeanObject = LeanNat.ofLong(v.toLong() and 0xFFFFFFFFL)

    @JvmStatic
    public fun boxUInt64(v: Long): LeanObject =
        if (v >= 0L) LeanNat.ofLong(v) else LeanNat.ofBigInteger(bigIntFromULong(v))

    @JvmStatic
    public fun boxUSize(v: Long): LeanObject =
        if (v >= 0L) LeanNat.ofLong(v) else LeanNat.ofBigInteger(bigIntFromULong(v))

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

    @JvmStatic
    public fun ioResultGetValue(obj: LeanObject?): LeanObject? {
        if (obj is LeanCtor) {
            return obj.getObj(0)
        }
        return obj
    }

    @JvmStatic
    public fun handleIOResult(obj: LeanObject?) {
        if (obj is LeanCtor && obj.tag != 0) {
            val err = obj.getObj(0)
            System.err.println("Uncaught Lean exception: " + err)
            System.exit(1)
        }
    }

    @JvmStatic
    public fun stringArrayToList(args: Array<String>): LeanObject {
        var list: LeanObject = LeanNat.ZERO
        for (i in args.indices.reversed()) {
            val s = LeanString.of(args[i])
            val cons = LeanCtor.alloc(1, 2, 0)
            cons.setObj(0, s)
            cons.setObj(1, list)
            list = cons
        }
        return list
    }

    @JvmStatic
    public fun initializeModule(className: String) {
        try {
            val cls = Class.forName(className.replace('/', '.'))
            try {
                val m = cls.getMethod("initialize")
                m.invoke(null)
            } catch (_: NoSuchMethodException) {
            }
        } catch (_: ClassNotFoundException) {
        } catch (e: java.lang.reflect.InvocationTargetException) {
            val cause = e.cause ?: e
            if (cause is RuntimeException) throw cause
            throw RuntimeException(cause)
        }
    }
}
