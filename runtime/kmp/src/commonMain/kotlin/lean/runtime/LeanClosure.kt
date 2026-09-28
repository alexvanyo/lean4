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
    public fun apply(vararg args: LeanObject?): LeanObject? {
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

    public fun apply1(a1: LeanObject?): LeanObject? {
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

    public fun apply2(a1: LeanObject?, a2: LeanObject?): LeanObject? {
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

    public fun apply3(a1: LeanObject?, a2: LeanObject?, a3: LeanObject?): LeanObject? {
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

