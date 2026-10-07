package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class InlinedLoopsTest {
    @Test
    fun testComputeSum() {
        assertEquals(0u, f_computeSum(0u))
        assertEquals(1u, f_computeSum(1u))
        assertEquals(55u, f_computeSum(10u))
    }

    @Test
    fun testSearchWithEarlyReturn() {
        assertEquals(0, f_searchWithEarlyReturn(10u, 0u))
        assertEquals(5, f_searchWithEarlyReturn(10u, 5u))
        assertEquals(9, f_searchWithEarlyReturn(10u, 9u))
        assertEquals(-1, f_searchWithEarlyReturn(10u, 10u))
        assertEquals(-1, f_searchWithEarlyReturn(10u, 100u))
    }

    @Test
    fun testNestedBooleanLoops() {
        assertEquals(true, f_allBelowLimit(3u, 4u, 12u))
        assertEquals(false, f_allBelowLimit(3u, 4u, 11u))
        assertEquals(false, f_allBelowLimit(3u, 4u, 5u))
        assertEquals(true, f_anyEquals(3u, 4u, 0u))
        assertEquals(true, f_anyEquals(3u, 4u, 7u))
        assertEquals(true, f_anyEquals(3u, 4u, 11u))
        assertEquals(false, f_anyEquals(3u, 4u, 12u))
    }
}
