/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

import kotlin.jvm.JvmField
import kotlin.jvm.Volatile

/**
 * Representation of a Lean mutable reference cell (ST.Ref / IO.Ref).
 */
public final class LeanRef(
    @Volatile @JvmField public var value: LeanObject?
) : LeanObject() {
    public override val tag: Int get() = 0

    override fun toString(): String = "Ref(" + value.toString() + ")"
}
