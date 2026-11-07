package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

@OptIn(ExperimentalUnsignedTypes::class)
class TypedArraysSetOptionTest {
    @Test
    fun testTypedSignatures() {
        assertEquals(6uL, f_TypedArraysSetOption_sumCells(ulongArrayOf(1uL, 2uL, 3uL)))
        val b: CellBatch = f_TypedArraysSetOption_mkBatch(4uL)
        val cells: ULongArray = b.cells
        assertEquals(listOf(4uL, 5uL), cells.toList())
        assertEquals(9uL, f_TypedArraysSetOption_total(b))
    }

    @Test
    fun testEmptyArrays() {
        val e = f_TypedArraysSetOption_emptyBatch()
        assertEquals(0, e.cells.size)
        assertEquals(0, e.warnings.size)
        assertEquals(0uL, f_TypedArraysSetOption_total(e))
        assertEquals(0, f_TypedArraysSetOption_emptyBatchOf(1u).first.cells.size)
    }
}
