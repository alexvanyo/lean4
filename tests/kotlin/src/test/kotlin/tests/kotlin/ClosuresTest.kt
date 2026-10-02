package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class ClosuresTest {
    private fun doubleVal(x: UInt): UInt = x * 2u

    @Test
    fun testApplyTwiceWithLambdaAndFunctionRef() {
        assertEquals(16u, f_applyTwice({ it + 3u }, 10u))
        assertEquals(20u, f_applyTwice(::doubleVal, 5u))
    }

    @Test
    fun testMakeAdder() {
        assertEquals(15u, f_makeAdder(5u, 10u))
    }

    @Test
    fun testClosureApp() {
        assertEquals(20u, f_testClosureApp(10u))
    }

    @Test
    fun testNoInlineClosure() {
        assertEquals(16u, f_applyTwiceNoInline({ it + 3u }, 10u))
        assertEquals(20u, f_testNoInlineClosure(10u))
    }

    @Test
    fun testApplyBinary() {
        assertEquals(13u, f_applyBinary({ a, b -> a * b + 1u }, 3u, 4u))
    }

    @Test
    fun testChooseFn() {
        assertEquals(11u, f_chooseFn(true, { it + 1u }, { it * 2u }, 10u))
        assertEquals(20u, f_chooseFn(false, { it + 1u }, { it * 2u }, 10u))
    }

    @Test
    fun testApplyErased() {
        val erasedFn: Any? = { x: UInt -> x + 7u }
        assertEquals(17u, f_applyErased(erasedFn, 10u))
    }
}
