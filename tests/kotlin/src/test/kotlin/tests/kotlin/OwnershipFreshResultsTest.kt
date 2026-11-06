package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class OwnershipFreshResultsTest {
    private fun list(l: Any?): List<Any?> {
        val out = ArrayList<Any?>()
        var cur = l as Array<*>
        while (cur[0] as Int == 1) {
            out.add(cur[1])
            cur = cur[2] as Array<*>
        }
        return out
    }

    @Test
    fun testClosedConstantForTwoExclusiveArgs() {
        val (a, b) = f_OwnershipFresh_twoEmpties(3u)
        assertEquals(listOf<Any?>(3u, 2u, 1u), list(a))
        assertEquals(listOf<Any?>(30u, 20u, 10u), list(b))
    }

    @Test
    fun testTupleOfFreshBuffers() {
        val (b, w) = f_OwnershipFresh_runOuter(2u)
        assertEquals(listOf<Any?>(3u, 2u, 1u, 3u, 2u, 1u), list(b))
        assertEquals(listOf<Any?>(2u, 2u), list(w))
    }
}
