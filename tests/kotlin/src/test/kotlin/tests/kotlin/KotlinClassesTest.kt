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

        val p2 = p.translate(5, -3)
        assertEquals(15, p2.x)
        assertEquals(17, p2.y)
        // Original immutable Point is unchanged
        assertEquals(10, p.x)
        assertEquals(20, p.y)

        val p3 = f_makePoint(3, 7)
        assertEquals(3, p3.x)
        assertEquals(7, p3.y)
    }

    @Test
    fun testMutableBuffer() {
        val buf = Buffer(8, 16)
        buf.grow(16)
        assertEquals(8, buf.size)
        assertEquals(32, buf.capacity)
        buf.clear()
        assertEquals(0, buf.size)
        assertEquals(32, buf.capacity)
    }
}
