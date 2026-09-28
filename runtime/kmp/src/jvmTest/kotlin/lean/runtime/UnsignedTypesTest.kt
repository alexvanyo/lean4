/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

import lean.mod_l_Init_Data_UInt_Basic
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertSame
import kotlin.test.assertTrue

class UnsignedTypesTest {

    @Test
    fun testSmallNatCache() {
        for (i in 0..255) {
            val a = LeanNat.ofLong(i.toLong())
            val b = LeanNat.ofLong(i.toLong())
            assertSame(a, b, "Values 0..255 should be cached singletons")
            assertEquals(i.toULong(), a.smallVal)
        }
        val over = LeanNat.ofLong(256L)
        val over2 = LeanNat.ofLong(256L)
        assertEquals(256uL, over.smallVal)
        assertFalse(over === over2, "Values >= 256 should allocate fresh instances")
    }

    @Test
    fun testBoxUnboxUnsigned() {
        val u8: UByte = 250u
        val boxedU8 = LeanRuntimeJVM.boxUInt8(u8)
        assertEquals(250uL, (boxedU8 as LeanNat).smallVal)
        assertSame(LeanNat.ofLong(250L), boxedU8)
        assertEquals(u8, LeanRuntimeJVM.unboxUInt8(boxedU8))

        val u16: UShort = 60000u
        val boxedU16 = LeanRuntimeJVM.boxUInt16(u16)
        assertEquals(60000uL, (boxedU16 as LeanNat).smallVal)
        assertEquals(u16, LeanRuntimeJVM.unboxUInt16(boxedU16))

        val u32: UInt = 3500000000u
        val boxedU32 = LeanRuntimeJVM.boxUInt32(u32)
        assertEquals(3500000000uL, (boxedU32 as LeanNat).smallVal)
        assertEquals(u32, LeanRuntimeJVM.unboxUInt32(boxedU32))

        val u64: ULong = 18446744073709551615uL
        val boxedU64 = LeanRuntimeJVM.boxUInt64(u64) as LeanNat
        kotlin.test.assertNull(boxedU64.bigVal, "UInt64.MAX should fit in smallVal without BigInteger")
        assertEquals(u64, boxedU64.smallVal)
        assertEquals(u64, LeanRuntimeJVM.unboxUInt64(boxedU64))
    }

    @Test
    fun testTopBitDoesNotRequireBigInteger() {
        // 2^63 has the top bit set (0x8000_0000_0000_0000uL)
        val twoPow63: ULong = 1uL shl 63
        val nat1 = LeanNat.ofULong(twoPow63)
        kotlin.test.assertNull(nat1.bigVal, "2^63 must fit in smallVal without BigInteger")
        assertEquals(twoPow63, nat1.smallVal)

        // 2^64 - 1 (18446744073709551615uL) has all 64 bits set
        val maxU64 = ULong.MAX_VALUE
        val nat2 = LeanNat.ofULong(maxU64)
        kotlin.test.assertNull(nat2.bigVal, "2^64 - 1 must fit in smallVal without BigInteger")
        assertEquals(maxU64, nat2.smallVal)

        // Arithmetic between large values with top bit set
        // (2^64 - 1) - (2^63) = 2^63 - 1
        val sub = nat2.sub(nat1)
        kotlin.test.assertNull(sub.bigVal, "Result should fit in smallVal without BigInteger")
        assertEquals(maxU64 - twoPow63, sub.smallVal)

        // Only numbers >= 2^64 require BigInteger
        val big = LeanNat.ofDecString("18446744073709551616")
        kotlin.test.assertNotNull(big.bigVal, "Numbers >= 2^64 require BigInteger")
    }

    @Test
    fun testUInt8OperationsUseCachedNats() {
        val a = LeanRuntimeJVM.boxUInt8(200u)
        val b = LeanRuntimeJVM.boxUInt8(100u)

        // 200 + 100 = 300 = 44 mod 256
        val sum = mod_l_Init_Data_UInt_Basic.f_UInt8_add(a, b)
        assertSame(LeanNat.ofLong(44L), sum, "UInt8 addition result must be a cached singleton")

        // 100 - 200 = -100 = 156 mod 256
        val diff = mod_l_Init_Data_UInt_Basic.f_UInt8_sub(b, a)
        assertSame(LeanNat.ofLong(156L), diff, "UInt8 subtraction result must be a cached singleton")

        // 200 * 2 = 400 = 144 mod 256
        val prod = mod_l_Init_Data_UInt_Basic.f_UInt8_mul(a, LeanRuntimeJVM.boxUInt8(2u))
        assertSame(LeanNat.ofLong(144L), prod, "UInt8 multiplication result must be a cached singleton")
    }

    @Test
    fun testUInt32UnsignedComparisons() {
        val a = LeanRuntimeJVM.boxUInt32(3000000000u)
        val b = LeanRuntimeJVM.boxUInt32(2000000000u)

        // In signed 32-bit int, 3000000000 would be negative, but unsigned comparison must see a > b
        val lt = mod_l_Init_Data_UInt_Basic.f_UInt32_decLt(b, a)
        assertSame(LeanNat.ONE, lt)

        val le = mod_l_Init_Data_UInt_Basic.f_UInt32_decLe(a, b)
        assertSame(LeanNat.ZERO, le)

        val div = mod_l_Init_Data_UInt_Basic.f_UInt32_div(a, b)
        assertEquals(1uL, (div as LeanNat).smallVal)

        val rem = mod_l_Init_Data_UInt_Basic.f_UInt32_mod(a, b)
        assertEquals(1000000000uL, (rem as LeanNat).smallVal)
    }

    @Test
    fun testLeanByteArrayWithUnsigned() {
        var ba = LeanByteArray.empty()
        ba = ba.push(42u.toUByte())
        ba = ba.push(255u.toUByte())

        assertEquals(2, ba.size())
        assertEquals(42u.toUByte(), ba.getUByte(0))
        assertEquals(255u.toUByte(), ba.getUByte(1))

        ba = ba.set(0, 200u.toUByte())
        assertEquals(200u.toUByte(), ba.getUByte(0))
    }
}
