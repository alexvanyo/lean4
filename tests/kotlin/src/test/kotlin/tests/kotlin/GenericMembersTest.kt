package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class GenericMembersTest {
    @Test
    fun testTypeParameterBeforeReceiver() {
        val counter = GenericCounter(5u, 0uL)
        val old: UInt
        val new: UInt
        counter.bumpBefore(2u) { a, b ->
            old = a
            new = b
        }
        assertEquals(5u, old)
        assertEquals(7u, new)
        assertEquals(7u, counter.count)
        assertEquals(2uL, counter.total)
    }

    @Test
    fun testTypeParameterAfterReceiver() {
        val counter = GenericCounter(5u, 1uL)
        val sum = counter.bumpAfter(3u) { a, b -> a.toLong() + b.toLong() }
        assertEquals(13L, sum)
        assertEquals(8u, counter.count)
        assertEquals(4uL, counter.total)
    }

    @Test
    fun testTypeParameterAfterRuntimeParameter() {
        assertEquals(65uL, f_applyAfterTwice(4u))
    }

    @Test
    fun testPartialApplicationSkipsTypeParameter() {
        val f1: (UInt) -> ULong = { it.toULong() }
        val f2: (UInt) -> ULong = { it.toULong() * 2uL }
        val fs: Any? = arrayOf<Any?>(1, f1, arrayOf<Any?>(1, f2, arrayOf<Any?>(0)))
        var list = f_applyAfterEach(fs) as Array<*>
        val results = mutableListOf<Any?>()
        while (list[0] == 1) {
            results.add(list[1])
            list = list[2] as Array<*>
        }
        assertEquals(listOf<Any?>(4uL, 8uL), results)
    }
}
