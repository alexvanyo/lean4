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

    @Test
    fun testOption() {
        assertEquals(52u, f_testOption(41u))
        val noneVal = f_optionMapInc(arrayOf<Any?>(0))
        assertEquals(99u, f_optionGetD(noneVal, 99u))
        val someVal = f_optionMapInc(arrayOf<Any?>(1, 10u))
        assertEquals(11u, f_optionGetD(someVal, 99u))
    }

    @Test
    fun testList() {
        val xs = f_makeList3(10u, 20u, 30u)
        assertEquals(60u, f_sumList(xs))
    }

    @Test
    fun testExcept() {
        val okVal = f_makeExcept(true, 42u)
        val errVal = f_makeExcept(false, 42u)
        assertEquals(42u, f_exceptToCode(okVal))
        assertEquals(999u, f_exceptToCode(errVal))
    }

    @Test
    fun testBoxedItem() {
        val item = f_mkBoxedItem("widget", 5u, 2.5)
        assertEquals("widget", f_boxedItemTag(item))
        assertEquals(5u, f_boxedItemCount(item))
        assertEquals(2.5, f_boxedItemWeight(item))
        val bumped = f_bumpBoxedItem(item, 3u)
        assertEquals("widget", f_boxedItemTag(bumped))
        assertEquals(8u, f_boxedItemCount(bumped))
        assertEquals(2.5, f_boxedItemWeight(bumped))
    }

    @Test
    fun testExprTree() {
        val tree = f_sampleTree(25, 10)
        assertEquals(15, f_evalTree(tree))
    }
}
