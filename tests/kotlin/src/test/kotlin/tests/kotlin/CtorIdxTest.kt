package tests.kotlin

import java.math.BigInteger
import kotlin.test.Test
import kotlin.test.assertEquals

/** `T.ctorIdx` implementations (`getObjTagNat`) on each Kotlin representation of an inductive. */
class CtorIdxTest {
    private fun n(i: Int) = BigInteger.valueOf(i.toLong())

    @Test
    fun testEnum() {
        assertEquals(n(2), f_TrafficLight_ctorIdx___impl(TrafficLight.Yellow))
        assertEquals(n(1), f_TrafficLight_ctorIdx___impl___boxed(TrafficLight.Green))
        assertEquals(n(2), f_Color_ctorIdx___impl(2u.toUByte()))
    }

    @Test
    fun testSealed() {
        assertEquals(n(2), f_ArithExpr_ctorIdx___impl(ArithExpr.Neg(ArithExpr.Lit(1))))
        assertEquals(n(0), f_Shape_ctorIdx___impl(Shape.Empty))
        assertEquals(n(2), f_Shape_ctorIdx___impl___boxed(Shape.Rect(1.0, 2.0)))
        assertEquals(n(3), f_JsonValue_ctorIdx___impl(JsonValue.StrVal("s")))
    }

    @Test
    fun testGeneric() {
        assertEquals(n(0), f_CustomList_ctorIdx___impl(CustomList.Nil))
        assertEquals(n(1), f_CustomList_ctorIdx___impl___boxed(null, CustomList.Cons(1, CustomList.Nil)))
    }

    @Test
    fun testAdt() {
        assertEquals(n(1), f_ExprTree_ctorIdx___impl(arrayOf<Any?>(1, null)))
    }
}
