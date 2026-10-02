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
}
