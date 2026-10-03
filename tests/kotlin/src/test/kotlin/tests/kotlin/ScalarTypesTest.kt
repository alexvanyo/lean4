package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertTrue

class ScalarTypesTest {
    @Test
    fun testUInts() {
        val res = f_testUInts(10u.toUByte(), 20u.toUShort(), 30u, 100uL, 0)
        assertTrue(res > 0uL)
    }

    @Test
    fun testInts() {
        val res = f_testInts((10).toByte(), (20).toShort(), 30, 100L, 0)
        assertTrue(res > 0L)
    }

    @Test
    fun testBools() {
        assertTrue(f_testBools(true, true))
        assertTrue(f_testBools(false, false))
        assertEquals(false, f_testBools(true, false))
        assertEquals(false, f_testBools(false, true))
    }

    @Test
    fun testFloats() {
        val res = f_testFloats(2.5, 1.0f)
        // sum = 4.0, diff = 3.5, prod = 7.0, quot = 1.75, neg = -1.75
        // abs(neg) = 1.75, sqrt(9.0) = 3.0, floor(2.7) = 2.0, ceil(2.3) = 3.0, c = 1.0 -> 10.75
        assertEquals(10.75, res, 0.001)
    }

    @Test
    fun testFloat32s() {
        val res = f_testFloat32s(1.5f, 3.0f)
        // x = (4.5 - 0.5) * 2.0 / 4.0 = 2.0f, y = -2.0f, abs(y) = 2.0f
        assertEquals(2.0f, res, 0.001f)
    }

    @Test
    fun testLeanInts() {
        val res = f_testLeanInts(-7, 3)
        // sum = -4, diff = -7, prod = 14, q = 4, r = 2, eq = -3, er = 2, neg = -4, absVal = 7
        // -7 < 3 is true -> -4 + 2 + (-3) + 2 + 7 = 4
        assertEquals(4, (res as Number).toInt())
    }

    @Test
    fun testNatOps() {
        val res = f_testNatOps(12.toBigInteger(), 18.toBigInteger())
        assertEquals(230.toBigInteger(), res)
    }

    @Test
    fun testLargeNatAndBitVec() {
        val n = 2.toBigInteger()
        val expected = n.pow(100) + java.math.BigInteger("1000000000000000000000000000000") - java.math.BigInteger.ONE
        assertEquals(expected, f_testLargeNat(n))

        val a = java.math.BigInteger("18446744073709551615") // 2^64 - 1
        val b = java.math.BigInteger.TWO
        // (a + b) mod 2^64 = 1; (a <<< 3) mod 2^64 = 2^64 - 8 = 18446744073709551608; 1 xor ... = 18446744073709551609
        assertEquals(java.math.BigInteger("18446744073709551609"), f_testBitVec64(a, b))
    }

    @Test
    fun testPanicOpt() {
        assertEquals(42.toBigInteger(), f_testPanicOpt(arrayOf<Any?>(1, 42.toBigInteger())))
        assertFailsWith<IllegalStateException> {
            f_testPanicOpt(arrayOf<Any?>(0))
        }
    }
}
