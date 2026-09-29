/-!
Tests that `Option` of non-nullable types (`Nat`, `String`), nested `Option`, and primitive `Option UInt32` execute correctly and soundly.
-/

def optNat (x : Nat) : Option Nat := Option.some x

def matchOpt (x : Option Nat) : Nat :=
  match x with
  | Option.none => 42
  | Option.some v => v

def optString (x : String) : Option String := Option.some x

def matchOptString (x : Option String) : String :=
  match x with
  | Option.none => "none"
  | Option.some v => v

def optOptNat (x : Option Nat) : Option (Option Nat) := Option.some x

def matchOptOptNat (x : Option (Option Nat)) : Nat :=
  match x with
  | Option.none => 999
  | Option.some (Option.none) => 888
  | Option.some (Option.some v) => v

def optUInt32 (x : UInt32) : Option UInt32 := Option.some x

def matchOptUInt32 (x : Option UInt32) : UInt32 :=
  match x with
  | Option.none => 123
  | Option.some v => v

def main : IO Unit := do
  IO.println (matchOpt (optNat 100))
  IO.println (matchOpt Option.none)
  IO.println (matchOptString (optString "hello"))
  IO.println (matchOptString Option.none)
  IO.println (matchOptOptNat (optOptNat (optNat 777)))
  IO.println (matchOptOptNat (optOptNat Option.none))
  IO.println (matchOptOptNat Option.none)
  IO.println (matchOptUInt32 (optUInt32 99))
  IO.println (matchOptUInt32 Option.none)
