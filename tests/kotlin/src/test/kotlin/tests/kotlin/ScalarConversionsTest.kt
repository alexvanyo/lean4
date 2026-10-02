package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class ScalarConversionsTest {
    @Test
    fun testConversions() {
        val res = f_testConversions(10, -5, 100uL, 200L)
        assertEquals(325, (res as Number).toInt())
    }

    @Test
    fun testDirectCast() {
        val res = f_testDirectCast(100u, 50)
        assertEquals(150uL, res)
    }

    @Test
    fun testFloatConversions() {
        val res = f_testFloatConversions(3.5, 2.25f, 10u, -4)
        // u = 3u, s = 3, s32 = 2
        // f_from_u = 3.0 + 10.0 = 13.0
        // f_from_s = 3.0 + (-4.0) + 2.0 = 1.0
        // f32_sum = 3.5f + 10.0f + (-4.0f) = 9.5f
        // total = 23.5
        assertEquals(23.5, res, 0.001)
    }
}
