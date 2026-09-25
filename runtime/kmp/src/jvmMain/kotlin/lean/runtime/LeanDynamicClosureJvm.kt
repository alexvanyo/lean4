/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime

import java.lang.invoke.MethodHandle
import java.lang.invoke.MethodHandles
import java.lang.reflect.Modifier
import java.util.concurrent.ConcurrentHashMap

private val CLOSURE_CACHE = ConcurrentHashMap<String, MethodHandle>()

internal actual fun invokeDynamicClosure(
    targetClass: String,
    targetMethod: String,
    arity: Int,
    args: Array<LeanObject?>
): LeanObject? {
    if (targetClass.isEmpty() || targetMethod.isEmpty()) {
        throw IllegalStateException("Dynamic closure has no target function defined")
    }
    val key = "$targetClass#$targetMethod#$arity"
    var mh = CLOSURE_CACHE[key]
    if (mh == null) {
        val jvmClassName = targetClass.replace('/', '.')
        val cl = Thread.currentThread().contextClassLoader ?: LeanClosure::class.java.classLoader
        val clazz = Class.forName(jvmClassName, true, cl)
        for (m in clazz.declaredMethods) {
            if (m.name == targetMethod && Modifier.isStatic(m.modifiers) && m.parameterCount == arity) {
                m.isAccessible = true
                mh = MethodHandles.lookup().unreflect(m)
                CLOSURE_CACHE[key] = mh
                break
            }
        }
        if (mh == null) {
            throw NoSuchMethodException("Static method $targetMethod with $arity params not found in $jvmClassName")
        }
    }

    val res = mh.invokeWithArguments(*args)
    return when (res) {
        is LeanObject -> res
        is Long -> LeanNat.ofLong(res)
        is Int -> LeanNat.ofLong(res.toLong())
        null -> null
        else -> LeanString.of(res.toString())
    }
}
