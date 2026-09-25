/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

import kotlin.jvm.JvmStatic
import kotlin.jvm.Synchronized

/**
 * Representation of a lazy Lean Thunk.
 */
public final class LeanThunk : LeanObject {
    private var value: LeanObject? = null
    private var closure: LeanClosure? = null

    public constructor(closure: LeanClosure?) {
        this.closure = closure
        this.value = null
    }

    public constructor(value: LeanObject?) {
        this.closure = null
        this.value = value
    }

    @Synchronized
    public fun get(): LeanObject? {
        val v = value
        if (v != null) return v
        val c = closure
        if (c != null) {
            val res = if (c.captured.size < c.arity) c.apply(null) else c.apply()
            value = res
            closure = null
            return res
        }
        return null
    }

    companion object {
        @JvmStatic
        public fun alloc(closure: LeanClosure?): LeanThunk = LeanThunk(closure)

        @JvmStatic
        public fun pure(value: LeanObject?): LeanThunk = LeanThunk(value)
    }
}
