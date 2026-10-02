package tests.kotlin

import kotlin.toUByte as stdlibToUByte
import kotlin.toUShort as stdlibToUShort
import kotlin.toUInt as stdlibToUInt
import kotlin.toULong as stdlibToULong

@Suppress("NOTHING_TO_INLINE", "UNCHECKED_CAST")
inline operator fun Any?.invoke(vararg args: Any?): Any? {
    return (this as Function<*>).let {
        when (it) {
            is Function0<*> -> it()
            is Function1<*, *> -> (it as (Any?) -> Any?)(args[0])
            is Function2<*, *, *> -> (it as (Any?, Any?) -> Any?)(args[0], args[1])
            else -> error("Unsupported function arity: $it")
        }
    }
}

fun f_Float_ofScientific(m: Any?, s: Boolean, e: Any?): Double {
    val mantissa = (m as Number).toDouble()
    val exponent = (e as Number).toDouble()
    val scale = Math.pow(10.0, if (s) -exponent else exponent)
    return mantissa * scale
}

fun f_Float_add(a: Double, b: Double): Double = a + b

private fun toJvmNumber(a: Any?): java.lang.Number = when (a) {
    is java.lang.Number -> a
    is UByte -> a.toByte() as java.lang.Number
    is UShort -> a.toShort() as java.lang.Number
    is UInt -> a.toInt() as java.lang.Number
    is ULong -> a.toLong() as java.lang.Number
    else -> java.lang.Long.parseLong(a.toString()) as java.lang.Number
}

fun Any?.toByte(): Byte = toJvmNumber(this).byteValue()
fun Any?.toShort(): Short = toJvmNumber(this).shortValue()
fun Any?.toInt(): Int = toJvmNumber(this).intValue()
fun Any?.toLong(): Long = toJvmNumber(this).longValue()

fun Any?.toUByte(): UByte = when (this) {
    is UByte -> this
    else -> toJvmNumber(this).byteValue().stdlibToUByte()
}

fun Any?.toUShort(): UShort = when (this) {
    is UShort -> this
    else -> toJvmNumber(this).shortValue().stdlibToUShort()
}

fun Any?.toUInt(): UInt = when (this) {
    is UInt -> this
    else -> toJvmNumber(this).intValue().stdlibToUInt()
}

fun Any?.toULong(): ULong = when (this) {
    is ULong -> this
    else -> toJvmNumber(this).longValue().stdlibToULong()
}
