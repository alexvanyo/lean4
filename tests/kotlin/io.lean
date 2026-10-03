/-!
Tests `BaseIO` and `IO` runtime primitives (`IO.mkRef`, `IO.Ref` operations, `IO.monoMsNow`,
`IO.monoNanosNow`, `IO.getEnv`, `IO.initializing`, `IO.println`, `IO.print`, `IO.eprintln`,
`IO.withIsolatedStreams`, and `EIO` exception handling) in the Kotlin backend.
-/

def testBaseIORef (x y : Nat) : BaseIO (Nat × Nat × Bool) := do
  let r1 ← IO.mkRef x
  let r2 ← IO.mkRef y
  let eq1 ← r1.ptrEq r1
  let eq2 ← r1.ptrEq r2
  r1.modify (· + 10)
  let old ← r1.swap (← r2.get)
  let cur ← r1.modifyGet fun v => (v + 1, v * 2)
  let final ← r1.get
  return (old + cur, final, eq1 && !eq2)

def testBaseIOClockAndInit : BaseIO (Bool × Bool × Bool) := do
  let init ← IO.initializing
  let canceled ← IO.checkCanceled
  let t1 ← IO.monoMsNow
  let t2 ← IO.monoMsNow
  let n1 ← IO.monoNanosNow
  let n2 ← IO.monoNanosNow
  return (!init && !canceled, t2 >= t1, n2 >= n1 && n1 > 0)

def testBaseIOEnv : BaseIO (Bool × Bool) := do
  let path? ← IO.getEnv "PATH"
  let missing? ← IO.getEnv "LEAN_KOTLIN_NONEXISTENT_ENV_VAR_99999"
  return (path?.isSome, missing?.isNone)

def testIOPrintCaptured (msg : String) : IO (String × Nat) := do
  IO.FS.withIsolatedStreams do
    IO.print "Hello, "
    IO.println msg
    IO.eprintln "err-line"
    return msg.length

def testIOException (ok : Bool) : BaseIO String := do
  let act : IO String := do
    if ok then
      return "success"
    else
      throw (IO.userError "boom")
  match ← act.toBaseIO with
  | .ok s => return s
  | .error e => return toString e
