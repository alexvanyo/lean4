/*
 * Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
 * Released under Apache 2.0 license as described in the file LICENSE.
 */
package lean.runtime;

public abstract class LeanClosure extends LeanObject {
    public final int arity;
    public final LeanObject[] captured;

    public static final LeanObject[] EMPTY_CAPTURED = new LeanObject[0];

    public LeanClosure(int arity, LeanObject[] captured) {
        this.arity = arity;
        this.captured = captured != null ? captured : EMPTY_CAPTURED;
    }

    public abstract LeanObject invokeBody(LeanObject[] args);

    public abstract LeanClosure copyCurried(LeanObject[] newCaptured);

    public LeanObject apply(LeanObject... args) {
        int total = captured.length + args.length;
        if (total == arity) {
            LeanObject[] full = new LeanObject[arity];
            System.arraycopy(captured, 0, full, 0, captured.length);
            System.arraycopy(args, 0, full, captured.length, args.length);
            return invokeBody(full);
        } else if (total < arity) {
            LeanObject[] newCap = new LeanObject[total];
            System.arraycopy(captured, 0, newCap, 0, captured.length);
            System.arraycopy(args, 0, newCap, captured.length, args.length);
            return copyCurried(newCap);
        } else {
            int needed = arity - captured.length;
            LeanObject[] first = new LeanObject[needed];
            System.arraycopy(args, 0, first, 0, needed);
            LeanObject[] rem = new LeanObject[args.length - needed];
            System.arraycopy(args, needed, rem, 0, rem.length);

            LeanClosure intermediate = (LeanClosure) apply(first);
            return intermediate.apply(rem);
        }
    }

    public LeanObject apply1(LeanObject a1) {
        return apply(new LeanObject[] { a1 });
    }

    public LeanObject apply2(LeanObject a1, LeanObject a2) {
        return apply(new LeanObject[] { a1, a2 });
    }

    public LeanObject apply3(LeanObject a1, LeanObject a2, LeanObject a3) {
        return apply(new LeanObject[] { a1, a2, a3 });
    }

    public static LeanClosure alloc(int arity) {
        return alloc(null, null, arity, EMPTY_CAPTURED);
    }

    public static LeanClosure alloc(String className, String methodName, int arity, LeanObject[] captured) {
        return new LeanDynamicClosure(className, methodName, arity, captured);
    }

    public static class LeanDynamicClosure extends LeanClosure {
        public final String targetClass;
        public final String targetMethod;
        private volatile java.lang.invoke.MethodHandle methodHandle;

        private static final java.util.concurrent.ConcurrentHashMap<String, java.lang.invoke.MethodHandle> CACHE =
                new java.util.concurrent.ConcurrentHashMap<>();

        public LeanDynamicClosure(String targetClass, String targetMethod, int arity, LeanObject[] captured) {
            super(arity, captured);
            this.targetClass = targetClass;
            this.targetMethod = targetMethod;
        }

        @Override
        public LeanClosure copyCurried(LeanObject[] newCaptured) {
            LeanDynamicClosure c = new LeanDynamicClosure(targetClass, targetMethod, arity, newCaptured);
            c.methodHandle = this.methodHandle;
            return c;
        }

        private java.lang.invoke.MethodHandle resolveHandle() {
            if (targetClass == null || targetMethod == null) {
                throw new IllegalStateException("Dynamic closure has no target function defined");
            }
            String key = targetClass + "#" + targetMethod + "#" + arity;
            java.lang.invoke.MethodHandle mh = CACHE.get(key);
            if (mh != null) return mh;

            try {
                String jvmClassName = targetClass.replace('/', '.');
                ClassLoader cl = Thread.currentThread().getContextClassLoader();
                if (cl == null) cl = LeanClosure.class.getClassLoader();
                Class<?> clazz = Class.forName(jvmClassName, true, cl);
                for (java.lang.reflect.Method m : clazz.getDeclaredMethods()) {
                    if (m.getName().equals(targetMethod) &&
                            java.lang.reflect.Modifier.isStatic(m.getModifiers()) &&
                            m.getParameterCount() == arity) {
                        m.setAccessible(true);
                        mh = java.lang.invoke.MethodHandles.lookup().unreflect(m);
                        CACHE.put(key, mh);
                        return mh;
                    }
                }
                throw new NoSuchMethodException("Static method " + targetMethod + " with " + arity + " params not found in " + jvmClassName);
            } catch (Exception e) {
                throw new RuntimeException("Failed to resolve closure target " + targetClass + "." + targetMethod, e);
            }
        }

        @Override
        public LeanObject invokeBody(LeanObject[] args) {
            java.lang.invoke.MethodHandle mh = methodHandle;
            if (mh == null) {
                mh = resolveHandle();
                methodHandle = mh;
            }
            try {
                Object res = mh.invokeWithArguments((Object[]) args);
                if (res instanceof LeanObject) {
                    return (LeanObject) res;
                } else if (res instanceof Long) {
                    return LeanNat.ofLong((Long) res);
                } else if (res instanceof Integer) {
                    return LeanNat.ofLong(((Integer) res).longValue());
                } else if (res == null) {
                    return null;
                } else {
                    return LeanString.of(res.toString());
                }
            } catch (Throwable t) {
                if (t instanceof RuntimeException) throw (RuntimeException) t;
                throw new RuntimeException("Error invoking closure " + targetClass + "." + targetMethod, t);
            }
        }
    }
}
