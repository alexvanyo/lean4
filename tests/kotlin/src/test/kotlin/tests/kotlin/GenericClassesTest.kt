package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class GenericClassesTest {
    @Test
    fun testGenericBox() {
        val b: Box<Int> = f_makeBox(42)
        val v: Int = f_unbox(b)
        assertEquals(42, v)
        assertEquals(42, b.extractVal())

        val bStr: Box<String> = f_mapBox({ x: Int -> "num: $x" }, b)
        assertEquals("num: 42", bStr.`val`)

        val bStr2: Box<String> = b.mapVal { x: Int -> "val: $x" }
        assertEquals("val: 42", bStr2.`val`)
    }

    @Test
    fun testGenericKeyValue() {
        val kv: KeyValue<String, Int> = f_makeKeyValue("age", 30)
        assertEquals("age", kv.key)
        assertEquals(30, kv.value)

        val swapped: KeyValue<Int, String> = f_swapKeyValue(kv)
        assertEquals(30, swapped.key)
        assertEquals("age", swapped.value)
    }

    @Test
    fun testGenericCustomList() {
        val empty: CustomList<Int> = f_makeCustomNil()
        assertEquals(CustomList.Nil, empty)
        assertEquals(99, f_customHeadD(empty, 99))

        val list: CustomList<Int> = f_makeCustomCons(10, empty)
        assertTrue(list is CustomList.Cons)
        assertEquals(10, f_customHeadD(list, 99))
    }

    @Test
    fun testNestedGenericBox() {
        val nested: Box<Box<Int>> = f_wrapBox(100)
        val inner: Box<Int> = nested.`val`
        assertEquals(100, inner.`val`)
    }

    @Test
    fun testGenericMapHolder() {
        val holder: GenericMapHolder<Int> = f_makeGenericMapHolder("answer", 42)
        assertEquals(42, f_lookupGenericMap(holder, "answer", 0))
        assertEquals(0, f_lookupGenericMap(holder, "missing", 0))
        assertEquals(42, holder.lookup("answer", 0))
        assertEquals(0, holder.lookup("missing", 0))
    }

    @Test
    fun testGenericTreeMapHolder() {
        val treeHolder: GenericTreeMapHolder<String> = f_makeGenericTreeMapHolder("fruit", "apple")
        assertEquals("apple", f_lookupGenericTreeMap(treeHolder, "fruit", "default"))
        assertEquals("default", f_lookupGenericTreeMap(treeHolder, "other", "default"))
    }

    @Test
    fun testGenericMapOperations() {
        val emptyMap: Any? = f_makeEmptyMap<String>()
        val m1: Any? = f_genericMapInsert(emptyMap, "greeting", "hello")
        assertEquals("hello", f_genericMapGet(m1, "greeting", "fallback"))
        assertEquals("fallback", f_genericMapGet(m1, "unknown", "fallback"))
    }

    @Test
    fun testMapWithBoxAndList() {
        val mapWithBox = f_mapWithBox("score", 100)
        assertEquals(100, f_getBoxValFromMap(mapWithBox, "score", 0))
        assertEquals(0, f_getBoxValFromMap(mapWithBox, "nonexistent", 0))

        val mapWithList = f_mapWithCustomList("primes", 7)
        assertEquals(7, f_getHeadFromMap(mapWithList, "primes", 0))
        assertEquals(0, f_getHeadFromMap(mapWithList, "empty", 0))
    }

    @Test
    fun testSetsWithGenerics() {
        val b1 = Box("alpha")
        val b2 = Box("beta")
        val set = f_makeBoxSet(b1, b2)
        assertTrue(f_boxSetContains(set, Box("alpha")))
        assertTrue(f_boxSetContains(set, Box("beta")))
        assertFalse(f_boxSetContains(set, Box("gamma")))

        val treeSet = f_makeTreeSetWith("first")
        assertTrue(f_treeSetContains(treeSet, "first"))
        assertFalse(f_treeSetContains(treeSet, "second"))
    }
}
