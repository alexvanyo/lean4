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

    @Test
    fun testSynthesizedPerson() {
        val p = f_makePerson("Alice", java.math.BigInteger.valueOf(30L), 100)
        assertEquals("Alice", p.name)
        assertEquals(java.math.BigInteger.valueOf(30L), p.age)
        assertEquals(100, p.score)
        assertEquals("Alice:30:100", p.summary())

        val p2 = p.birthday().withScore(150)
        assertEquals("Alice", p2.name)
        assertEquals(java.math.BigInteger.valueOf(31L), p2.age)
        assertEquals(150, p2.score)
        // Original immutable Person is unchanged
        assertEquals(java.math.BigInteger.valueOf(30L), p.age)
        assertEquals(100, p.score)
    }

    @Test
    fun testUnboxedSingleFieldImmutableClass() {
        val sum: Int = f_addMeters(12, 30)
        assertEquals(42, sum)
        val scaled: Int = f_scaleMeters(sum, 3)
        assertEquals(126, scaled)
    }

    @Test
    fun testSingleFieldMutableCounter() {
        val c = Counter(10)
        assertEquals(10, c.readCount())
        c.inc(5)
        assertEquals(15, c.count)
        assertEquals(15, c.readCount())
        c.inc(-3)
        assertEquals(12, c.count)
    }

    @Test
    fun testObjectFieldMutablePerson() {
        val mp = MutablePerson("Bob", java.math.BigInteger.valueOf(40L))
        mp.birthday()
        assertEquals("Bob", mp.name)
        assertEquals(java.math.BigInteger.valueOf(41L), mp.age)
        mp.rename("Robert")
        assertEquals("Robert", mp.name)
        assertEquals(java.math.BigInteger.valueOf(41L), mp.age)
    }
}
