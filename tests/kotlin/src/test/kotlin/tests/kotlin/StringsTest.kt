package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class StringsTest {
    @Test
    fun testStringEscape() {
        val expected = "line1\nline2\t\"quoted\" \$dollar \\backslash"
        assertEquals(expected, f_testStringEscape())
    }

    @Test
    fun testStringConcat() {
        val res: String = f_testStringConcat("hello", "world")
        assertEquals("hello - world", res)
    }

    @Test
    fun testStringMetrics() {
        assertEquals(0, (f_testStringMetrics("") as Number).toInt())
        // "abc" -> length 3, utf8ByteSize 3 -> 6
        assertEquals(6, (f_testStringMetrics("abc") as Number).toInt())
    }

    @Test
    fun testStringCompare() {
        assertTrue(f_testStringCompare("abc", "abc"))
        assertTrue(f_testStringCompare("abc", "abd"))
        assertFalse(f_testStringCompare("abd", "abc"))
    }
}
