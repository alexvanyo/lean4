package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class StrictAndOrTest {
    @Test
    fun testStrictAnd() {
        assertTrue(f_myStrictAnd(true, true))
        assertFalse(f_myStrictAnd(true, false))
        assertFalse(f_myStrictAnd(false, true))
        assertFalse(f_myStrictAnd(false, false))
    }

    @Test
    fun testStrictOr() {
        assertTrue(f_myStrictOr(true, true))
        assertTrue(f_myStrictOr(true, false))
        assertTrue(f_myStrictOr(false, true))
        assertFalse(f_myStrictOr(false, false))
    }

    @Test
    fun testCondBranch() {
        assertEquals(5u, f_condBranch(10u, 5u))
        assertEquals(20u, f_condBranch(10u, 10u))
        assertEquals(5u, f_condBranch(0u, 5u))
        assertEquals(0u, f_condBranch(5u, 10u))
    }
}
