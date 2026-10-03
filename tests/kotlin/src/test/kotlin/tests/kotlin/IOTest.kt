package tests.kotlin

import java.math.BigInteger
import kotlin.test.Test
import kotlin.test.assertEquals

class IOTest {
    @Test
    fun testBaseIORef() {
        // r1 starts at 5, r2 at 20
        // r1.modify (+10) -> r1 = 15
        // old = r1.swap(r2.get) -> old = 15, r1 = 20
        // cur = r1.modifyGet (v + 1, v * 2) -> cur = 21, r1 = 40
        // final = r1.get = 40
        // returns (old + cur = 36, final = 40, eq1 && !eq2 = true)
        val res = f_testBaseIORef(5.toBigInteger(), 20.toBigInteger(), null) as Pair<*, *>
        val sum = res.first as BigInteger
        val p2 = res.second as Pair<*, *>
        val finalVal = p2.first as BigInteger
        val ptrOk = p2.second as Boolean
        assertEquals(36.toBigInteger(), sum)
        assertEquals(40.toBigInteger(), finalVal)
        assertEquals(true, ptrOk)
    }

    @Test
    fun testBaseIOClockAndInit() {
        val res = f_testBaseIOClockAndInit(null) as Pair<*, *>
        val p2 = res.second as Pair<*, *>
        assertEquals(true, res.first as Boolean)
        assertEquals(true, p2.first as Boolean)
        assertEquals(true, p2.second as Boolean)
    }

    @Test
    fun testBaseIOEnv() {
        val res = f_testBaseIOEnv(null) as Pair<*, *>
        assertEquals(true, res.first as Boolean)
        assertEquals(true, res.second as Boolean)
    }

    @Test
    fun testIOPrintCaptured() {
        val out = f_testIOPrintCaptured("LeanKotlin", null) as Array<*>
        assertEquals(0, out[0] as Int)
        val pair = out[1] as Pair<*, *>
        val captured = pair.first as String
        val len = pair.second as BigInteger
        assertEquals("Hello, LeanKotlin\nerr-line\n", captured)
        assertEquals(10.toBigInteger(), len)
    }

    @Test
    fun testIOException() {
        assertEquals("success", f_testIOException(true, null) as String)
        assertEquals("boom", f_testIOException(false, null) as String)
    }
}
