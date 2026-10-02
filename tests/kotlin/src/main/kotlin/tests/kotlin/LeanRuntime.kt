package tests.kotlin

import kotlin.toUByte as stdlibToUByte
import kotlin.toUShort as stdlibToUShort
import kotlin.toUInt as stdlibToUInt
import kotlin.toULong as stdlibToULong

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
