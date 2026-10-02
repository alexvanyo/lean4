package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class TupleProjTest {
    @Test
    fun testTriple() {
        val t = Triple(10, 20, 30)
        assertEquals(10, t.fst)
        assertEquals(20, t.snd)
        assertEquals(30, t.thd)
        assertEquals(60, t.sumFields())
    }
}
