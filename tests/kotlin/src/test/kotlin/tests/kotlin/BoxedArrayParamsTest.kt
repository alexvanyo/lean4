package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class BoxedArrayParamsTest {
    @Test
    fun testBoxedWrapperCastsObjectArrays() {
        val xs: Any? = arrayOf<Any?>(1u, 2u, 3u)
        val ys: Any? = arrayOf<Any?>(10uL, 20uL, 30uL)
        assertEquals(35uL, f_sumFrom___boxed(xs, ys, java.math.BigInteger.valueOf(2), 2u))
    }

    @Test
    fun testBoxedWrapperCastsScalarArrays() {
        val bs: Any? = byteArrayOf(1, 5, 9)
        val fs: Any? = doubleArrayOf(0.5, 1.5, 2.5)
        assertEquals(3u, f_countAbove___boxed(bs, fs, (4u).toUByte(), 2.0))
    }
}
