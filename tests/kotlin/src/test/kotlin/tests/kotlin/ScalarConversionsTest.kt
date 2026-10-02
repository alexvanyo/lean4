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
}
