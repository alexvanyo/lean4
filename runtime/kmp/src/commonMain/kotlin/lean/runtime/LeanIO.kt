/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

import kotlin.jvm.JvmField
import kotlin.jvm.JvmStatic

/**
 * Representation of IO results in Lean:
 * EStateM.Result ε σ α = ok (a : α) (s : σ) | error (e : ε) (s : σ)
 */
public final class LeanIOResult(
    public val isOk: Boolean,
    @JvmField public val value: LeanObject?,
    @JvmField public val error: LeanObject?
) : LeanObject() {

    companion object {
        @JvmStatic
        public fun ok(value: LeanObject?): LeanIOResult = LeanIOResult(true, value, null)

        @JvmStatic
        public fun error(error: LeanObject?): LeanIOResult = LeanIOResult(false, null, error)
    }
}
