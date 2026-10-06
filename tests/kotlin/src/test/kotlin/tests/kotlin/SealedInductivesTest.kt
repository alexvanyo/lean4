package tests.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class SealedInductivesTest {
    @Test
    fun testEnumTrafficLight() {
        assertEquals(1u, f_lightToCode(TrafficLight.Red))
        assertEquals(2u, f_lightToCode(TrafficLight.Green))
        assertEquals(3u, f_lightToCode(TrafficLight.Yellow))

        assertEquals(TrafficLight.Red, f_makeLight(1u))
        assertEquals(TrafficLight.Green, f_makeLight(2u))
        assertEquals(TrafficLight.Yellow, f_makeLight(3u))

        val names = TrafficLight.entries.map { it.name }
        assertEquals(listOf("Red", "Green", "Yellow"), names)
    }

    @Test
    fun testPlainSealedInterfaceArithExpr() {
        val tree = f_sampleArith(25, 10)
        assertTrue(tree is ArithExpr.Add)
        val left = tree.l
        val right = tree.r
        assertTrue(left is ArithExpr.Lit)
        assertEquals(25, left.v)
        assertTrue(right is ArithExpr.Neg)
        val negInner = right.e
        assertTrue(negInner is ArithExpr.Lit)
        assertEquals(10, negInner.v)

        assertEquals(15, f_evalArith(tree))
        assertEquals(15, tree.eval())
    }

    @Test
    fun testDataSealedInterfaceShape() {
        val empty = f_makeEmpty()
        assertEquals(Shape.Empty, empty)
        assertEquals(0.0, f_area(empty))

        val circle = f_makeCircle(2.0)
        assertTrue(circle is Shape.Circle)
        assertEquals(2.0, circle.radius)
        assertEquals(3.141592653589793 * 4.0, f_area(circle))

        // Data class copy and equals
        val circle2 = circle.copy(radius = 3.0)
        assertEquals(3.0, circle2.radius)
        assertEquals("Circle(radius=2.0)", circle.toString())

        val rect = f_makeRect(4.0, 5.0)
        assertTrue(rect is Shape.Rect)
        assertEquals(4.0, rect.width)
        assertEquals(5.0, rect.height)
        assertEquals(20.0, f_area(rect))
        assertEquals("Rect(width=4.0, height=5.0)", rect.toString())
    }

    @Test
    fun testDataSealedClassJsonValue() {
        val nullVal = f_makeJsonNull()
        assertEquals(JsonValue.NullVal, nullVal)
        assertEquals("null", f_jsonToString(nullVal))

        val boolVal = f_makeJsonBool(true)
        assertTrue(boolVal is JsonValue.BoolVal)
        assertEquals(true, boolVal.b)
        assertEquals("true", f_jsonToString(boolVal))

        val numVal = f_makeJsonNum(42)
        assertTrue(numVal is JsonValue.NumVal)
        assertEquals(42, numVal.n)
        assertEquals("42", f_jsonToString(numVal))

        val strVal = f_makeJsonStr("hello")
        assertTrue(strVal is JsonValue.StrVal)
        assertEquals("hello", strVal.s)
        assertEquals("\"hello\"", f_jsonToString(strVal))
        assertEquals("StrVal(s=hello)", strVal.toString())
    }

    @Test
    fun testDataClassVec2() {
        val a = Vec2(3, 7)
        val b = Vec2(2, 5)
        val c = f_addVec2(a, b)
        assertEquals(Vec2(5, 12), c)
        assertEquals(5, c.x)
        assertEquals(12, c.y)
        assertEquals("Vec2(x=5, y=12)", c.toString())
        val (x, y) = c
        assertEquals(5, x)
        assertEquals(12, y)
    }
}
