package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class KotlinExprTemplateTest {
    @Test
    fun testTemplates() {
        assertEquals(10u + 15u + 4u, f_KotlinExprTemplate_useTemplates(5u))
        assertEquals(6u, f_KotlinExprTemplate_dbl(3u))
        assertEquals(2u, f_KotlinExprTemplate_sub(3u, 1u))
    }
}
