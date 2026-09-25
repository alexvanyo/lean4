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
    @JvmField public val captured: Array<LeanObject?> = EMPTY_CAPTURED
) : LeanObject() {

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
                for (i in captured.indices) fullArgs[i] = captured[i]
                for (i in args.indices) fullArgs[captured.size + i] = args[i]
                invokeBody(fullArgs)
            }
            total < arity -> {
                val newCaptured = arrayOfNulls<LeanObject>(total)
                for (i in captured.indices) newCaptured[i] = captured[i]
                for (i in args.indices) newCaptured[captured.size + i] = args[i]
                copyCurried(newCaptured)
            }
            else -> {
                // Over-application: satisfy arity first, then apply remaining to the resulting closure
                val needed = arity - captured.size
                val firstBatch = arrayOfNulls<LeanObject>(needed)
                for (i in 0 until needed) firstBatch[i] = args[i]
                val remaining = arrayOfNulls<LeanObject>(args.size - needed)
                for (i in needed until args.size) remaining[i - needed] = args[i]

                val intermediate = apply(*firstBatch) as LeanClosure
                intermediate.apply(*remaining)
            }
        }
    }

    public fun apply1(a1: LeanObject?): LeanObject? = apply(a1)
    public fun apply2(a1: LeanObject?, a2: LeanObject?): LeanObject? = apply(a1, a2)
    public fun apply3(a1: LeanObject?, a2: LeanObject?, a3: LeanObject?): LeanObject? = apply(a1, a2, a3)

    companion object {
        @JvmField
        public val EMPTY_CAPTURED: Array<LeanObject?> = emptyArray()
    }
}
