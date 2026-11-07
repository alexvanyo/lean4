package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class ErasedSignednessTest {
    @Test
    fun testBoxedInt32Components() {
        val cells = f_ErasedSignedness_pushCell(arrayOf(), 3u, 0xFFFFFFFFu)
        val p = cells[0] as Pair<*, *>
        assertEquals(3, p.first as Int)
        assertEquals(-1, p.second as Int)
        val q = f_ErasedSignedness_mkCell(5u, 6u)
        assertEquals(5, q.first as Int)
        assertEquals(6, q.second as Int)
    }

    @Test
    fun testWidening() {
        assertEquals(0xFFFFFFFFuL, f_ErasedSignedness_widenSnd(Pair(0, -1)))
        assertEquals(0x1_0000_0000uL, f_ErasedSignedness_widen(-1))
        assertEquals(-1L, f_ErasedSignedness_narrowToNat(0xFFu))
    }
}
