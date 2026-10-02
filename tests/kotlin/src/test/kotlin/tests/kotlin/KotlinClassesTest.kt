package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class KotlinClassesTest {
    @Test
    fun testPointProperties() {
        val p = Point(10, 20)
        assertEquals(10, p.getXCoord())
        assertEquals(10, p.x)
        assertEquals(20, p.y)
    }
}
