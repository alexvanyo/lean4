package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class BoxedSignednessTest {
    @Test
    fun testInt32PairElementsWithEitherBoxing() {
        assertEquals("-3 4", f_BoxedSignedness_formatAt(arrayOf<Any?>(Pair(-3, 4)), 0u))
        assertEquals("-3 4", f_BoxedSignedness_formatAt(arrayOf<Any?>(Pair((-3).toUInt(), 4u)), 0u))
        assertEquals(-4, f_BoxedSignedness_callFirstMinusOne(arrayOf<Any?>(Pair(-3, 4))))
        assertEquals(-4, f_BoxedSignedness_callFirstMinusOne(arrayOf<Any?>(Pair((-3).toUInt(), 4u))))
    }

    @Test
    fun testToInt32InField() {
        assertEquals(7, f_BoxedSignedness_parsedVal(7u, 1u))
        assertEquals(-1, f_BoxedSignedness_parsedVal(UInt.MAX_VALUE, 1u))
        assertEquals(0, f_BoxedSignedness_parsedVal(7u, 0u))
    }

    @Test
    fun testOptionPair() {
        assertEquals(7, f_BoxedSignedness_sumPair(true, 3, 4))
        assertEquals(0, f_BoxedSignedness_sumPair(false, 3, 4))
    }

    @Test
    fun testSharedLiteral() {
        val w = f_BoxedSignedness_mkSignedWarn(5u)
        assertEquals(5u, w.tag)
        assertEquals(0u, w.charVal)
        assertEquals(0, w.offsetX)
        assertEquals(0, w.offsetY)
    }
}
