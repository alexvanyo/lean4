/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

/**
 * Base class for all Lean heap objects in the runtime.
 */
public abstract class LeanObject {
    /** Constructor or object tag (0 for scalar/unboxed or default). */
    public open val tag: Int get() = 0

    /** True if this object represents an unboxed/scalar value. */
    public open val isScalar: Boolean get() = false

    override fun equals(other: Any?): Boolean {
        return this === other
    }

    override fun hashCode(): Int {
        return super.hashCode()
    }
}
