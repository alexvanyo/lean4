package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class JoinPointsTest {
    @Test
    fun testJoinPointAliases() {
        assertEquals(31u, f_testJoinPointAliases(true, 10u, 5u))
        assertEquals(11u, f_testJoinPointAliases(false, 10u, 5u))
    }

    @Test
    fun testNestedJoinPoints() {
        assertEquals(30, f_testNestedJoinPoints(true, true, 10, 5, 2))
        assertEquals(10, f_testNestedJoinPoints(true, false, 10, 5, 2))
        assertEquals(14, f_testNestedJoinPoints(false, true, 10, 5, 2))
        assertEquals(6, f_testNestedJoinPoints(false, false, 10, 5, 2))
    }

    @Test
    fun testPreserveLet() {
        assertEquals(15u, f_testPreserveLet(0u))
        assertEquals(25u, f_testPreserveLet(10u))
    }
}
