package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class TypedArraysTest {
    @OptIn(ExperimentalUnsignedTypes::class)
    @Test
    fun testU64Array() {
        val arr = f_makeU64Array(5.toBigInteger())
        assertEquals(5.toBigInteger(), f_getArraySize(arr))
        assertEquals(0uL, f_readArrayU64(arr, 0, null))
        assertEquals(0uL, f_readArrayU64(arr, 4, null))
    }

    @OptIn(ExperimentalUnsignedTypes::class)
    @Test
    fun testU32Array() {
        val arr = f_makeU32Array(3.toBigInteger(), 42u)
        assertEquals(3, arr.size)
        assertEquals(42u, arr[0])
        assertEquals(42u, arr[1])
        assertEquals(42u, arr[2])
    }

    @Test
    fun testBoolArray() {
        val arr = f_makeBoolArray(4.toBigInteger())
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

        f_swapArrayU32(arr, 0.toBigInteger(), 2.toBigInteger(), null, null)
        assertEquals(30u, arr[0])
        assertEquals(20u, arr[1])
        assertEquals(10u, arr[2])

        f_swapIfInBoundsU32(arr, 0.toBigInteger(), 1.toBigInteger())
        assertEquals(20u, arr[0])
        assertEquals(30u, arr[1])

        // Out-of-bounds swapIfInBounds is a no-op
        f_swapIfInBoundsU32(arr, 0.toBigInteger(), 99.toBigInteger())
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

        val sliced = f_sliceU32(combined, 2.toBigInteger(), 5.toBigInteger())
        assertEquals(listOf(3u, 4u, 10u), sliced.toList())

        val clamped = f_sliceU32(combined, 5.toBigInteger(), 100.toBigInteger())
        assertEquals(listOf(20u, 30u), clamped.toList())

        val emptySlice = f_sliceU32(combined, 4.toBigInteger(), 2.toBigInteger())
        assertEquals(emptyList(), emptySlice.toList())

        val strArr = f_makeLiteralStr("hello", "world")
        assertEquals(listOf<Any?>("hello", "world"), strArr.toList())
    }

    @OptIn(ExperimentalUnsignedTypes::class)
    @Test
    fun testPolyArrayAndListConversions() {
        val strArr = f_makeLiteralStr("a", "b")
        val natArr: Array<Any?> = arrayOf(1.toBigInteger(), 5.toBigInteger(), 2.toBigInteger(), 8.toBigInteger(), 3.toBigInteger())
        assertEquals(19.toBigInteger(), f_sumNatArray(natArr))

        val filtered = f_filterNatArray(natArr, 4.toBigInteger())
        assertEquals(listOf<Any?>(5.toBigInteger(), 8.toBigInteger()), filtered.toList())

        val natList = f_arrayToListNat(filtered)
        val roundTripNat = f_listToArrayNat(natList)
        assertEquals(listOf<Any?>(5.toBigInteger(), 8.toBigInteger()), roundTripNat.toList())

        val u32Arr = f_makeLiteralU32(10u, 20u, 30u)
        val u32List = f_arrayToListU32(u32Arr)
        val roundTripU32 = f_listToArrayU32(u32List)
        assertEquals(listOf(10u, 20u, 30u), roundTripU32.toList())
        assertEquals(2, strArr.size)
    }

    @Test
    fun testByteArray() {
        val bs = f_makeByteArray(10u, 20u, 30u)
        assertEquals(3.toBigInteger(), f_byteArraySize(bs))
        assertEquals((10u).toUByte(), f_byteArrayGet_x21(bs, 0.toBigInteger()))
        assertEquals((20u).toUByte(), f_byteArrayGet_x21(bs, 1.toBigInteger()))
        assertEquals((30u).toUByte(), f_byteArrayGet_x21(bs, 2.toBigInteger()))

        val bs2 = f_byteArraySet_x21(bs, 1.toBigInteger(), 99u)
        assertEquals((20u).toUByte(), f_byteArrayGet_x21(bs, 1.toBigInteger()))
        assertEquals((99u).toUByte(), f_byteArrayGet_x21(bs2, 1.toBigInteger()))

        val combined = f_byteArrayAppend(bs, bs2)
        assertEquals(6.toBigInteger(), f_byteArraySize(combined))
        assertEquals(true, f_byteArrayEq(bs, f_byteArrayExtract(combined, 0.toBigInteger(), 3.toBigInteger())))
        assertEquals(false, f_byteArrayEq(bs, bs2))

        assertEquals("hello λ!", f_stringUTF8RoundTrip("hello λ!"))
        assertEquals(true, f_byteArrayValidateUTF8("hello λ!".encodeToByteArray()))
        assertEquals(false, f_byteArrayValidateUTF8(byteArrayOf(0xFF.toByte())))
    }

    @Test
    fun testFloatArray() {
        val ds = f_makeFloatArray(1.5, 2.5, 3.0)
        assertEquals(3.toBigInteger(), f_floatArraySize(ds))
        assertEquals(1.5, f_floatArrayGet_x21(ds, 0.toBigInteger()))
        assertEquals(2.5, f_floatArrayGet_x21(ds, 1.toBigInteger()))
        assertEquals(3.0, f_floatArrayGet_x21(ds, 2.toBigInteger()))
        assertEquals(7.0, f_sumFloatArray(ds))

        val ds2 = f_floatArraySet_x21(ds, 1.toBigInteger(), 10.5)
        assertEquals(2.5, f_floatArrayGet_x21(ds, 1.toBigInteger()))
        assertEquals(10.5, f_floatArrayGet_x21(ds2, 1.toBigInteger()))
        assertEquals(15.0, f_sumFloatArray(ds2))
    }
}
