package tests.kotlin

import java.math.BigInteger
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
        assertEquals(null, f_optionMapInc(null))
        assertEquals(99u, f_optionGetD(null, 99u))
        assertEquals(11u, f_optionMapInc(10u))
        assertEquals(10u, f_optionGetD(10u, 99u))
        assertEquals(0u, f_optionSomeFirst(null))
        assertEquals(20u, f_optionSomeFirst(10u))
        assertEquals("none", f_optionString(null))
        assertEquals("hello", f_optionString("hello"))
        assertEquals(0.toBigInteger(), f_optionNat(null))
        assertEquals(15.toBigInteger(), f_optionNat(5.toBigInteger()))
        // Nested option retains ADT Array<Any?> for outer option, inner is UInt?
        assertEquals(0u, f_nestedOption(arrayOf<Any?>(0)))
        assertEquals(1u, f_nestedOption(arrayOf<Any?>(1, null)))
        assertEquals(12u, f_nestedOption(arrayOf<Any?>(1, 10u)))
        // Generic option retains ADT Array<Any?>
        assertEquals(99u, f_genericOption(arrayOf<Any?>(0), 99u))
        assertEquals(42u, f_genericOption(arrayOf<Any?>(1, 42u), 99u))
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

    @Test
    fun testStdListOps() {
        val xs = f_makeList3(10u, 20u, 30u)
        assertEquals(3.toBigInteger(), f_listLength(xs))
        val rev = f_reverseList(xs)
        assertEquals(60u, f_sumList(rev))
        val ys = f_makeList3(1u, 2u, 3u)
        val combined = f_appendLists(xs, ys)
        assertEquals(6.toBigInteger(), f_listLength(combined))
        assertEquals(66u, f_sumList(combined))
        val mapped = f_mapListInc(xs)
        assertEquals(63u, f_sumList(mapped))
        val filtered = f_filterListGt(xs, 15u)
        assertEquals(2.toBigInteger(), f_listLength(filtered))
        assertEquals(50u, f_foldlListSum(filtered))
    }

    @Test
    fun testStdOptionOps() {
        assertEquals(24u, f_stdOptionMapGetD(12u, 99u))
        assertEquals(99u, f_stdOptionMapGetD(null, 99u))
    }
}
