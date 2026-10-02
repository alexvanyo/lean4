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

    @Test
    fun testPairs() {
        val p: Pair<UInt, UInt> = f_makePair(10u, 20u)
        assertEquals(Pair(11u, 22u), p)
        assertEquals(33u, f_sumPair(p))
        assertEquals(Pair("hello", 42u), f_swapPair(Pair(42u, "hello")))
        assertEquals(33u, f_roundTripPair(10u, 20u))
        val nested: Pair<UInt, Pair<Int, String>> = f_nestedPair(1u, -5, "nested")
        assertEquals(Pair(1u, Pair(-5, "nested")), nested)
        assertEquals("nested", f_readNestedPair(nested))
        assertEquals("", f_readNestedPair(Pair(0u, Pair(-5, "nested"))))
    }
}
