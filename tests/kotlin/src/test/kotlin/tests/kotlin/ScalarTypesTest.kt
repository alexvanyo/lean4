package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals
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
        assertEquals(4.0, res, 0.001)
    }
}
