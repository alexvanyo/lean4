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

    @Test
    fun testStringInterp() {
        assertEquals("score: nat=42, int=-7", f_testStringInterp("score", 42, -7))
    }

    @Test
    fun testStringChars() {
        assertEquals("HELLO, WORLD!", f_testStringChars("Hello, World!"))
    }

    @Test
    fun testStringExtract() {
        assertEquals("hello", f_testStringExtract("hello world"))
    }

    @Test
    fun testCharOps() {
        // 'A' (65) -> toNat = 65, utf8Size = 1, singleton.utf8ByteSize = 1 -> 67
        assertEquals(67, (f_testCharOps(65) as Number).toInt())
        // '∃' (8707) -> toNat = 8707, utf8Size = 3, singleton.utf8ByteSize = 3 -> 8713
        assertEquals(8713, (f_testCharOps(8707) as Number).toInt())
    }
}
