package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class TailrecLoopTest {
    @Test
    fun testCountUp() {
        assertEquals(10u, f_countUp(0u, 10u))
        assertEquals(12u, f_countUp(1u, 10u))
        assertEquals(20u, f_countUp(5u, 10u))
    }

    @Test
    fun testFoldBits() {
        assertEquals(0u, f_foldBits(0u, 0u))
        // 0b1011 (11) reversed into acc=0 should be 0b1101 (13)
        assertEquals(13u, f_foldBits(11u, 0u))
    }
}
