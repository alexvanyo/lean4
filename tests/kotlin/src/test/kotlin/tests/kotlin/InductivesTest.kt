package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class InductivesTest {
    @Test
    fun testColorToCode() {
        assertEquals(1u, f_colorToCode(0u.toUByte()))
        assertEquals(2u, f_colorToCode(1u.toUByte()))
        assertEquals(3u, f_colorToCode(2u.toUByte()))
    }

    @Test
    fun testCompareValues() {
        assertEquals(0u.toUByte(), f_compareValues(5u, 10u))
        assertEquals(1u.toUByte(), f_compareValues(10u, 10u))
        assertEquals(2u.toUByte(), f_compareValues(15u, 10u))
    }
}
