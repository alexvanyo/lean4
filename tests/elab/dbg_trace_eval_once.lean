/-!
Tests that an unused `@[never_extract]` application that is kept for its side effects is not also
merged into a later over-application, which would run `dbg_trace` twice.
-/

/-- info: dbg -/
#guard_msgs in
#eval show IO Unit from do dbg_trace "dbg"; pure ()
