import Lean.Compiler.Kotlin

/-!
Tests the ownership check on closed constants passed for several exclusive arguments (each use is
recomputed) and on fresh buffers returned in a tuple from a recursive helper.
-/

namespace OwnershipFresh

structure Buf (α : Type) where
  data : Array α
  size : UInt32

def Buf.push (b : Buf α) (x : α) : Buf α :=
  if b.size < b.data.size.toUInt32 then ⟨b.data.set! b.size.toNat x, b.size + 1⟩
  else ⟨b.data.push x, b.size + 1⟩

def Buf.toList (b : Buf UInt32) : List UInt32 := (b.data.extract 0 b.size.toNat).toList

def fill2 (n : UInt32) (a b : Buf UInt32) : List UInt32 × List UInt32 :=
  if h : n = 0 then (a.toList, b.toList)
  else
    have : (n - 1).toNat < n.toNat := by
      have : n.toNat ≠ 0 := fun h' => h (UInt32.toNat_inj.mp h')
      rw [UInt32.toNat_sub_of_le _ _ (by rw [UInt32.le_iff_toNat_le]; simp; omega)]; simp; omega
    fill2 (n - 1) (a.push n) (b.push (n * 10))
termination_by n.toNat

/-- Both empty buffers are the same closed constant. -/
def twoEmpties (n : UInt32) : List UInt32 × List UInt32 :=
  fill2 n ⟨#[], 0⟩ ⟨#[], 0⟩

/-- Returns its buffers, after pushing to them, in a tuple. -/
def lineHelper (k : UInt32) (b w : Buf UInt32) : Buf UInt32 × Buf UInt32 :=
  if h : k = 0 then (b, w)
  else
    have : (k - 1).toNat < k.toNat := by
      have : k.toNat ≠ 0 := fun h' => h (UInt32.toNat_inj.mp h')
      rw [UInt32.toNat_sub_of_le _ _ (by rw [UInt32.le_iff_toNat_le]; simp; omega)]; simp; omega
    lineHelper (k - 1) (b.push k) (if k % 2 == 0 then w.push k else w)
termination_by k.toNat

/-- Passes on the buffers returned by `lineHelper` to exclusive parameters. -/
def outer (n : UInt32) (b w : Buf UInt32) : List UInt32 × List UInt32 :=
  if h : n = 0 then (b.toList, w.toList)
  else
    have : (n - 1).toNat < n.toNat := by
      have : n.toNat ≠ 0 := fun h' => h (UInt32.toNat_inj.mp h')
      rw [UInt32.toNat_sub_of_le _ _ (by rw [UInt32.le_iff_toNat_le]; simp; omega)]; simp; omega
    let (b2, w2) := lineHelper 3 b w
    outer (n - 1) b2 w2
termination_by n.toNat

def runOuter (n : UInt32) : List UInt32 × List UInt32 :=
  outer n ⟨Array.emptyWithCapacity 4, 0⟩ ⟨Array.emptyWithCapacity 4, 0⟩

end OwnershipFresh
