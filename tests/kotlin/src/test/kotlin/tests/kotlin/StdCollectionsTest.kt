package tests.kotlin

import java.math.BigInteger
import kotlin.test.Test
import kotlin.test.assertEquals

class StdCollectionsTest {
    private fun optBigInt(o: Any?): BigInteger? {
        val arr = o as Array<*>
        return if (arr[0] as Int == 1) arr[1] as BigInteger else null
    }

    @Test
    fun testTreeMap() {
        val res = f_testTreeMap("beta", "alpha", "gamma", 10.toBigInteger(), 20.toBigInteger(), 30.toBigInteger())
        val p1 = optBigInt(res.first)
        val p2 = res.second
        val g2 = optBigInt(p2.first)
        val sz = p2.second
        assertEquals(10.toBigInteger(), p1)
        assertEquals(null, g2)
        assertEquals(2.toBigInteger(), sz)
    }

    @Test
    fun testTreeSet() {
        val res = f_testTreeSet("x", "y", "z")
        assertEquals(true, res.first)
        assertEquals(false, res.second.first)
        assertEquals(2.toBigInteger(), res.second.second)
    }

    @Test
    fun testHashMap() {
        val res = f_testHashMap("beta", "alpha", "gamma", 10.toBigInteger(), 20.toBigInteger(), 30.toBigInteger())
        assertEquals(10.toBigInteger(), optBigInt(res.first))
        assertEquals(null, optBigInt(res.second.first))
        assertEquals(2.toBigInteger(), res.second.second)
    }

    @Test
    fun testHashMapExpand() {
        val res = f_testHashMapExpand(32.toBigInteger())
        assertEquals(32.toBigInteger(), res.first)
        assertEquals(0.toBigInteger(), optBigInt(res.second.first))
        assertEquals(310.toBigInteger(), optBigInt(res.second.second))
    }

    @Test
    fun testHashSet() {
        val res = f_testHashSet("hello", "world", "lean")
        assertEquals(true, res.first)
        assertEquals(false, res.second.first)
        assertEquals(2.toBigInteger(), res.second.second)
    }

    @Test
    fun testScalarExtras() {
        // log2(8)=3, log2(16)=4, log2(32)=5, log2(64)=6 -> 18
        // abs(-5)=5, abs(-10)=10, abs(-20)=20, abs(-30)=30 -> 65
        // lt32 = 42 -> total = 18 + 65 + 42 = 125
        val res = f_testScalarExtras(
            8u.toUByte(), 16u.toUShort(), 32u, 64uL,
            (-5).toByte(), (-10).toShort(), -20, -30L
        )
        assertEquals(125uL, res)
    }

    @Test
    fun testFloatMinMaxNum() {
        val res = f_testFloatMinMaxNum(Double.NaN, 3.0, Float.NaN, 2.0f)
        // minimumNumber(NaN, 3.0) = 3.0, maximumNumber(NaN, 3.0) = 3.0
        // minimumNumber(NaN, 2.0f) = 2.0f, maximumNumber(NaN, 2.0f) = 2.0f
        assertEquals(10.0, res, 0.001)
    }

    @Test
    fun testByteSliceBeq() {
        val b1 = byteArrayOf(1, 2, 3)
        val b2 = byteArrayOf(1, 2, 3)
        val b3 = byteArrayOf(1, 2, 4)
        assertEquals(true, f_testByteSliceBeq(b1, b2))
        assertEquals(false, f_testByteSliceBeq(b1, b3))
    }

    @Test
    fun testStringSliceOps() {
        val r1 = f_testStringSliceOps("abc", "abc")
        assertEquals(true, r1.first)
        assertEquals(false, r1.second)

        val r2 = f_testStringSliceOps("abc", "abd")
        assertEquals(true, r2.second)
    }

    @Test
    fun testDbgTrace() {
        assertEquals(42.toBigInteger(), f_testDbgTrace(41.toBigInteger()))
    }
}
