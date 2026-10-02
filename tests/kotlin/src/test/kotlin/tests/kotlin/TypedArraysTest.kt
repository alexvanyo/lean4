package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class TypedArraysTest {
    @OptIn(ExperimentalUnsignedTypes::class)
    @Test
    fun testU64Array() {
        val arr = f_makeU64Array(5)
        assertEquals(5, f_getArraySize(arr))
        assertEquals(0uL, f_readArrayU64(arr, 0, null))
        assertEquals(0uL, f_readArrayU64(arr, 4, null))
    }

    @OptIn(ExperimentalUnsignedTypes::class)
    @Test
    fun testU32Array() {
        val arr = f_makeU32Array(3, 42u)
        assertEquals(3, arr.size)
        assertEquals(42u, arr[0])
        assertEquals(42u, arr[1])
        assertEquals(42u, arr[2])
    }

    @Test
    fun testBoolArray() {
        val arr = f_makeBoolArray(4)
        assertEquals(4, arr.size)
        assertEquals(false, arr[0])
    }

    @OptIn(ExperimentalUnsignedTypes::class)
    @Test
    fun testSwap() {
        val arr = f_makeLiteralU32(10u, 20u, 30u)
        assertEquals(3, arr.size)
        assertEquals(10u, arr[0])
        assertEquals(20u, arr[1])
        assertEquals(30u, arr[2])

        f_swapArrayU32(arr, 0, 2, null, null)
        assertEquals(30u, arr[0])
        assertEquals(20u, arr[1])
        assertEquals(10u, arr[2])

        f_swapIfInBoundsU32(arr, 0, 1)
        assertEquals(20u, arr[0])
        assertEquals(30u, arr[1])

        // Out-of-bounds swapIfInBounds is a no-op
        f_swapIfInBoundsU32(arr, 0, 99)
        assertEquals(20u, arr[0])
        assertEquals(30u, arr[1])
    }

    @OptIn(ExperimentalUnsignedTypes::class)
    @Test
    fun testPushPopAppendExtract() {
        val base = f_makeLiteralU32(1u, 2u, 3u)
        val pushedPopped = f_pushPopU32(base, 4u, 5u)
        assertEquals(listOf(1u, 2u, 3u, 4u), pushedPopped.toList())

        val other = f_makeLiteralU32(10u, 20u, 30u)
        val combined = f_appendU32(pushedPopped, other)
        assertEquals(listOf(1u, 2u, 3u, 4u, 10u, 20u, 30u), combined.toList())

        val sliced = f_sliceU32(combined, 2, 5)
        assertEquals(listOf(3u, 4u, 10u), sliced.toList())

        val clamped = f_sliceU32(combined, 5, 100)
        assertEquals(listOf(20u, 30u), clamped.toList())

        val emptySlice = f_sliceU32(combined, 4, 2)
        assertEquals(emptyList(), emptySlice.toList())

        val strArr = f_makeLiteralStr("hello", "world")
        assertEquals(listOf<Any?>("hello", "world"), strArr.toList())
    }
}
