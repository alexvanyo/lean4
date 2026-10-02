package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class ClosuresTest {
    @Test
    fun testMakeAdder() {
        assertEquals(15u, f_makeAdder(5u, 10u))
    }

    @Test
    fun testClosureApp() {
        assertEquals(20u, f_testClosureApp(10u))
    }
}
