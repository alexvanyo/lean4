/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

import kotlin.jvm.JvmField

/**
 * Representation of a Lean closure (partial application).
 * Supports automatic currying, partial application, and dynamic application.
 */
public abstract class LeanClosure(
    @JvmField public val arity: Int,
    captured: Array<LeanObject?>? = EMPTY_CAPTURED
) : LeanObject(),
    () -> LeanObject?,
    (LeanObject?) -> LeanObject?,
    (LeanObject?, LeanObject?) -> LeanObject?,
    (LeanObject?, LeanObject?, LeanObject?) -> LeanObject? {
    @JvmField public val captured: Array<LeanObject?> = captured ?: EMPTY_CAPTURED

    override fun invoke(): LeanObject? = apply()
    override fun invoke(p1: LeanObject?): LeanObject? = apply1(p1)
    override fun invoke(p1: LeanObject?, p2: LeanObject?): LeanObject? = apply2(p1, p2)
    override fun invoke(p1: LeanObject?, p2: LeanObject?, p3: LeanObject?): LeanObject? = apply3(p1, p2, p3)

    /**
     * Body invocation once all [arity] arguments are available.
     */
    public abstract fun invokeBody(args: Array<LeanObject?>): LeanObject?

    /**
     * Creates a new instance of this closure with additional curried arguments.
     */
    public abstract fun copyCurried(newCaptured: Array<LeanObject?>): LeanClosure

    /**
     * Applies [args] to the closure, returning the result if arity is reached,
     * a new curried closure if under-applied, or recursively applying remaining
     * arguments if over-applied.
     */
    public open fun apply(vararg args: LeanObject?): LeanObject? {
        val total = captured.size + args.size
        return when {
            total == arity -> {
                val fullArgs = arrayOfNulls<LeanObject>(arity)
                captured.copyInto(fullArgs)
                args.copyInto(fullArgs, destinationOffset = captured.size)
                invokeBody(fullArgs)
            }
            total < arity -> {
                val newCaptured = arrayOfNulls<LeanObject>(total)
                captured.copyInto(newCaptured)
                args.copyInto(newCaptured, destinationOffset = captured.size)
                copyCurried(newCaptured)
            }
            else -> {
                val needed = arity - captured.size
                val firstBatch = arrayOfNulls<LeanObject>(needed)
                args.copyInto(firstBatch, startIndex = 0, endIndex = needed)
                val remaining = arrayOfNulls<LeanObject>(args.size - needed)
                args.copyInto(remaining, destinationOffset = 0, startIndex = needed, endIndex = args.size)

                val intermediate = apply(*firstBatch) as LeanClosure
                intermediate.apply(*remaining)
            }
        }
    }

    public open fun apply1(a1: LeanObject?): LeanObject? {
        val total = captured.size + 1
        return when {
            total == arity -> {
                val fullArgs = arrayOfNulls<LeanObject>(arity)
                captured.copyInto(fullArgs)
                fullArgs[captured.size] = a1
                invokeBody(fullArgs)
            }
            total < arity -> {
                val newCaptured = arrayOfNulls<LeanObject>(total)
                captured.copyInto(newCaptured)
                newCaptured[captured.size] = a1
                copyCurried(newCaptured)
            }
            else -> apply(a1)
        }
    }

    public open fun apply2(a1: LeanObject?, a2: LeanObject?): LeanObject? {
        val total = captured.size + 2
        return when {
            total == arity -> {
                val fullArgs = arrayOfNulls<LeanObject>(arity)
                captured.copyInto(fullArgs)
                fullArgs[captured.size] = a1
                fullArgs[captured.size + 1] = a2
                invokeBody(fullArgs)
            }
            total < arity -> {
                val newCaptured = arrayOfNulls<LeanObject>(total)
                captured.copyInto(newCaptured)
                newCaptured[captured.size] = a1
                newCaptured[captured.size + 1] = a2
                copyCurried(newCaptured)
            }
            else -> apply(a1, a2)
        }
    }

    public open fun apply3(a1: LeanObject?, a2: LeanObject?, a3: LeanObject?): LeanObject? {
        val total = captured.size + 3
        return when {
            total == arity -> {
                val fullArgs = arrayOfNulls<LeanObject>(arity)
                captured.copyInto(fullArgs)
                fullArgs[captured.size] = a1
                fullArgs[captured.size + 1] = a2
                fullArgs[captured.size + 2] = a3
                invokeBody(fullArgs)
            }
            total < arity -> {
                val newCaptured = arrayOfNulls<LeanObject>(total)
                captured.copyInto(newCaptured)
                newCaptured[captured.size] = a1
                newCaptured[captured.size + 1] = a2
                newCaptured[captured.size + 2] = a3
                copyCurried(newCaptured)
            }
            else -> apply(a1, a2, a3)
        }
    }

    companion object {
        @JvmField
        public val EMPTY_CAPTURED: Array<LeanObject?> = emptyArray()

        @kotlin.jvm.JvmStatic
        public fun alloc(arity: Int): LeanClosure = alloc("", "", arity, EMPTY_CAPTURED)

        @kotlin.jvm.JvmStatic
        public fun alloc(
            className: String,
            methodName: String,
            arity: Int,
            captured: Array<LeanObject?>
        ): LeanClosure {
            return LeanDynamicClosure(className, methodName, arity, captured)
        }

        @kotlin.jvm.JvmStatic
        public fun ofFn0(fn: LeanFn0): LeanClosure = LeanFn0Closure(fn)

        @kotlin.jvm.JvmStatic
        public fun ofFn1(fn: LeanFn1): LeanClosure = LeanFn1Closure(fn)

        @kotlin.jvm.JvmStatic
        public fun ofFn2(fn: LeanFn2): LeanClosure = LeanFn2Closure(fn)

        @kotlin.jvm.JvmStatic
        public fun ofFn3(fn: LeanFn3): LeanClosure = LeanFn3Closure(fn)

        @kotlin.jvm.JvmStatic
        public fun ofFn4(fn: LeanFn4): LeanClosure = LeanFn4Closure(fn)

        @kotlin.jvm.JvmStatic
        public fun ofFn5(fn: LeanFn5): LeanClosure = LeanFn5Closure(fn)

        @kotlin.jvm.JvmStatic
        public fun ofFn6(fn: LeanFn6): LeanClosure = LeanFn6Closure(fn)

        @kotlin.jvm.JvmStatic
        public fun ofFn7(fn: LeanFn7): LeanClosure = LeanFn7Closure(fn)

        @kotlin.jvm.JvmStatic
        public fun ofFn8(fn: LeanFn8): LeanClosure = LeanFn8Closure(fn)
    }
}

public open class LeanDynamicClosure(
    public val targetClass: String,
    public val targetMethod: String,
    arity: Int,
    captured: Array<LeanObject?>?
) : LeanClosure(arity, captured) {
    override fun copyCurried(newCaptured: Array<LeanObject?>): LeanClosure {
        return LeanDynamicClosure(targetClass, targetMethod, arity, newCaptured)
    }

    override fun invokeBody(args: Array<LeanObject?>): LeanObject? {
        return invokeDynamicClosure(targetClass, targetMethod, arity, args)
    }
}

internal expect fun invokeDynamicClosure(
    targetClass: String,
    targetMethod: String,
    arity: Int,
    args: Array<LeanObject?>
): LeanObject?

public fun interface LeanFn0 { fun invoke(): LeanObject? }
public fun interface LeanFn1 { fun invoke(p1: LeanObject?): LeanObject? }
public fun interface LeanFn2 { fun invoke(p1: LeanObject?, p2: LeanObject?): LeanObject? }
public fun interface LeanFn3 { fun invoke(p1: LeanObject?, p2: LeanObject?, p3: LeanObject?): LeanObject? }
public fun interface LeanFn4 { fun invoke(p1: LeanObject?, p2: LeanObject?, p3: LeanObject?, p4: LeanObject?): LeanObject? }
public fun interface LeanFn5 { fun invoke(p1: LeanObject?, p2: LeanObject?, p3: LeanObject?, p4: LeanObject?, p5: LeanObject?): LeanObject? }
public fun interface LeanFn6 { fun invoke(p1: LeanObject?, p2: LeanObject?, p3: LeanObject?, p4: LeanObject?, p5: LeanObject?, p6: LeanObject?): LeanObject? }
public fun interface LeanFn7 { fun invoke(p1: LeanObject?, p2: LeanObject?, p3: LeanObject?, p4: LeanObject?, p5: LeanObject?, p6: LeanObject?, p7: LeanObject?): LeanObject? }
public fun interface LeanFn8 { fun invoke(p1: LeanObject?, p2: LeanObject?, p3: LeanObject?, p4: LeanObject?, p5: LeanObject?, p6: LeanObject?, p7: LeanObject?, p8: LeanObject?): LeanObject? }

public class LeanFn0Closure(
    @JvmField public val fn: LeanFn0,
    captured: Array<LeanObject?> = EMPTY_CAPTURED
) : LeanClosure(0, captured) {
    override fun invokeBody(args: Array<LeanObject?>): LeanObject? = fn.invoke()
    override fun copyCurried(newCaptured: Array<LeanObject?>): LeanClosure = LeanFn0Closure(fn, newCaptured)
}

public class LeanFn1Closure(
    @JvmField public val fn: LeanFn1,
    captured: Array<LeanObject?> = EMPTY_CAPTURED
) : LeanClosure(1, captured) {
    override fun invokeBody(args: Array<LeanObject?>): LeanObject? = fn.invoke(args[args.size - 1])
    override fun copyCurried(newCaptured: Array<LeanObject?>): LeanClosure = LeanFn1Closure(fn, newCaptured)
    override fun apply1(a1: LeanObject?): LeanObject? {
        return if (captured.isEmpty()) {
            fn.invoke(a1)
        } else {
            super.apply1(a1)
        }
    }
}

public class LeanFn2Closure(
    @JvmField public val fn: LeanFn2,
    captured: Array<LeanObject?> = EMPTY_CAPTURED
) : LeanClosure(2, captured) {
    override fun invokeBody(args: Array<LeanObject?>): LeanObject? =
        fn.invoke(args[args.size - 2], args[args.size - 1])
    override fun copyCurried(newCaptured: Array<LeanObject?>): LeanClosure = LeanFn2Closure(fn, newCaptured)
    override fun apply2(a1: LeanObject?, a2: LeanObject?): LeanObject? {
        return if (captured.isEmpty()) {
            fn.invoke(a1, a2)
        } else {
            super.apply2(a1, a2)
        }
    }
    override fun apply1(a1: LeanObject?): LeanObject? {
        return if (captured.size == 1) {
            fn.invoke(captured[0], a1)
        } else {
            super.apply1(a1)
        }
    }
}

public class LeanFn3Closure(
    @JvmField public val fn: LeanFn3,
    captured: Array<LeanObject?> = EMPTY_CAPTURED
) : LeanClosure(3, captured) {
    override fun invokeBody(args: Array<LeanObject?>): LeanObject? =
        fn.invoke(args[args.size - 3], args[args.size - 2], args[args.size - 1])
    override fun copyCurried(newCaptured: Array<LeanObject?>): LeanClosure = LeanFn3Closure(fn, newCaptured)
    override fun apply3(a1: LeanObject?, a2: LeanObject?, a3: LeanObject?): LeanObject? {
        return if (captured.isEmpty()) {
            fn.invoke(a1, a2, a3)
        } else {
            super.apply3(a1, a2, a3)
        }
    }
    override fun apply1(a1: LeanObject?): LeanObject? {
        return if (captured.size == 2) {
            fn.invoke(captured[0], captured[1], a1)
        } else {
            super.apply1(a1)
        }
    }
}

public class LeanFn4Closure(
    @JvmField public val fn: LeanFn4,
    captured: Array<LeanObject?> = EMPTY_CAPTURED
) : LeanClosure(4, captured) {
    override fun invokeBody(args: Array<LeanObject?>): LeanObject? =
        fn.invoke(args[args.size - 4], args[args.size - 3], args[args.size - 2], args[args.size - 1])
    override fun copyCurried(newCaptured: Array<LeanObject?>): LeanClosure = LeanFn4Closure(fn, newCaptured)
}

public class LeanFn5Closure(
    @JvmField public val fn: LeanFn5,
    captured: Array<LeanObject?> = EMPTY_CAPTURED
) : LeanClosure(5, captured) {
    override fun invokeBody(args: Array<LeanObject?>): LeanObject? =
        fn.invoke(args[args.size - 5], args[args.size - 4], args[args.size - 3], args[args.size - 2], args[args.size - 1])
    override fun copyCurried(newCaptured: Array<LeanObject?>): LeanClosure = LeanFn5Closure(fn, newCaptured)
}

public class LeanFn6Closure(
    @JvmField public val fn: LeanFn6,
    captured: Array<LeanObject?> = EMPTY_CAPTURED
) : LeanClosure(6, captured) {
    override fun invokeBody(args: Array<LeanObject?>): LeanObject? =
        fn.invoke(args[args.size - 6], args[args.size - 5], args[args.size - 4], args[args.size - 3], args[args.size - 2], args[args.size - 1])
    override fun copyCurried(newCaptured: Array<LeanObject?>): LeanClosure = LeanFn6Closure(fn, newCaptured)
}

public class LeanFn7Closure(
    @JvmField public val fn: LeanFn7,
    captured: Array<LeanObject?> = EMPTY_CAPTURED
) : LeanClosure(7, captured) {
    override fun invokeBody(args: Array<LeanObject?>): LeanObject? =
        fn.invoke(args[args.size - 7], args[args.size - 6], args[args.size - 5], args[args.size - 4], args[args.size - 3], args[args.size - 2], args[args.size - 1])
    override fun copyCurried(newCaptured: Array<LeanObject?>): LeanClosure = LeanFn7Closure(fn, newCaptured)
}

public class LeanFn8Closure(
    @JvmField public val fn: LeanFn8,
    captured: Array<LeanObject?> = EMPTY_CAPTURED
) : LeanClosure(8, captured) {
    override fun invokeBody(args: Array<LeanObject?>): LeanObject? =
        fn.invoke(args[args.size - 8], args[args.size - 7], args[args.size - 6], args[args.size - 5], args[args.size - 4], args[args.size - 3], args[args.size - 2], args[args.size - 1])
    override fun copyCurried(newCaptured: Array<LeanObject?>): LeanClosure = LeanFn8Closure(fn, newCaptured)
}


