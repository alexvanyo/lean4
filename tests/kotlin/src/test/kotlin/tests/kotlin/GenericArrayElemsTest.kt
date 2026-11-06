package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class GenericArrayElemsTest {
    @Test
    fun testAppend() {
        val r = f_GenericArrayElems_appendArrays(arrayOf<Any?>("a", "b"), arrayOf<Any?>("c"))
        assertEquals(listOf<Any?>("a", "b", "c"), r.toList())
        assertEquals(listOf<Any?>("c"), f_GenericArrayElems_appendArrays(arrayOf<Any?>(), arrayOf<Any?>("c")).toList())
    }

    @Test
    fun testClassElements() {
        val items = arrayOf<Any?>(ElemItem(42u, "x"), ElemItem(7u, "y"))
        assertEquals(42u, f_GenericArrayElems_firstA(arrayOf<Any?>(0, items, 2u)))
        assertEquals("y", f_GenericArrayElems_itemAtB(items, 1u))
    }
}
