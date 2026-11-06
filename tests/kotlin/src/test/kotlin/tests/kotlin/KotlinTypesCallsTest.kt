package tests.kotlin

import java.math.BigInteger
import kotlin.test.Test
import kotlin.test.assertEquals

class KotlinTypesCallsTest {
    @Test
    fun testErasedFunctionParameter() {
        val r = f_KotlinTypesCalls_stepWith(3u, StepState(0, 7, BigInteger.ZERO)) { s ->
            StepState(s.x, s.y + 2, s.gens.add(BigInteger.ONE))
        }
        assertEquals(13, r.y)
        assertEquals(BigInteger.valueOf(3), r.gens)
        val a = f_KotlinTypesCalls_advance(4u, StepState(-1, 0, BigInteger.ZERO))
        assertEquals(3, a.x)
        assertEquals(BigInteger.valueOf(4), a.gens)
    }
}
