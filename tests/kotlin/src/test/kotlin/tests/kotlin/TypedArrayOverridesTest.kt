package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals

class TypedArrayOverridesTest {
    @Test
    fun testTypedResults() {
        val input: Array<Pair<UInt, UInt>> = arrayOf(Pair(1u, 2u), Pair(3u, 4u))
        val out: Array<String> = f_TypedArrayOverrides_labels(input)
        assertContentEquals(arrayOf("1,2", "3,4"), out)
        // The argument is not updated in place.
        assertEquals(Pair(1u, 2u), input[0])
        assertContentEquals(arrayOf("0", "1", "2"), f_TypedArrayOverrides_names(3.toBigInteger()))
    }

    @Test
    fun testTypedField() {
        val n: Named = f_TypedArrayOverrides_mkNamed(2.toBigInteger())
        assertEquals(2u, n.count)
        val items: Array<String> = n.items
        assertContentEquals(arrayOf("0", "1"), items)
    }
}
