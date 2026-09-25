/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

import kotlin.jvm.JvmField
import kotlin.jvm.JvmStatic

/**
 * Representation of a Lean Array of LeanObjects.
 */
public final class LeanArray(
    @JvmField public var data: Array<LeanObject?>
) : LeanObject() {

    public val size: Int get() = data.size

    public fun get(index: Int): LeanObject? = data[index]
    public fun set(index: Int, value: LeanObject?) { data[index] = value }

    public fun push(value: LeanObject?): LeanArray {
        val newArr = arrayOfNulls<LeanObject>(data.size + 1)
        for (i in data.indices) newArr[i] = data[i]
        newArr[data.size] = value
        return LeanArray(newArr)
    }

    companion object {
        @JvmStatic
        public fun empty(): LeanArray = LeanArray(emptyArray())

        @JvmStatic
        public fun of(vararg elements: LeanObject?): LeanArray {
            val arr = arrayOfNulls<LeanObject>(elements.size)
            for (i in elements.indices) arr[i] = elements[i]
            return LeanArray(arr)
        }
    }
}
