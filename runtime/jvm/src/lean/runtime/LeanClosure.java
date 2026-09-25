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
}
