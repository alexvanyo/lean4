package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class CtorUnboxedFieldsTest {
    private fun leanList(vararg xs: Any?): Any? =
        xs.foldRight(arrayOf<Any?>(0) as Any?) { x, acc -> arrayOf<Any?>(1, x, acc) }

    private fun elements(list: Any?): List<Any?> {
        val out = mutableListOf<Any?>()
        var cur = list as Array<*>
        while (cur[0] == 1) {
            out.add(cur[1])
            cur = cur[2] as Array<*>
        }
        return out
    }

    @Test
    fun testKotlinClassFieldsFromBoxedElements() {
        val frames = elements(f_frameChildren(leanList(7u, 9u), leanList(0xF0uL), 100L, 8L, 5u))
        val a = frames[0] as Frame.Node
        assertEquals(7u, a.id)
        assertEquals(100L, a.x)
        assertEquals(4u, a.fuel)
        val b = frames[1] as Frame.Node
        assertEquals(9u, b.id)
        assertEquals(108L, b.x)
        assertEquals(4u, b.fuel)
        val c = frames[2] as Frame.Leaf
        assertEquals(0xF0uL, c.bits)
        assertEquals(108L, c.x)
    }

    @Test
    fun testAdtFieldsFromBoxedElements() {
        val cells = elements(f_cellChildren(leanList(3u, 4u), 10L, 2L))
        assertEquals(3u, f_cellId(cells[0]))
        assertEquals(10L, f_cellX(cells[0]))
        assertEquals(4u, f_cellId(cells[1]))
        assertEquals(12L, f_cellX(cells[1]))
        assertEquals(0u, f_cellId(cells[2]))
    }
}
