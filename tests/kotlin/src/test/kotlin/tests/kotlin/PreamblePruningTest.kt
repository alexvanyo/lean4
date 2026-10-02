package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals
import org.leanprover.demo.f_exportedAdd
import org.leanprover.demo.f_reachableHelper

class PreamblePruningTest {
    @Test
    fun testPreambleExported() {
        assertEquals(52u, f_reachableHelper(10u))
        assertEquals(72u, f_exportedAdd(10u, 20u))
    }
}
