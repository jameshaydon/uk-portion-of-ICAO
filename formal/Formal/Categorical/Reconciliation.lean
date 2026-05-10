import Formal.Categorical.Core
import Mathlib.CategoryTheory.Category.Basic

namespace Formal.Categorical

universe u v

namespace Reconciliation

/-- A functor that preserves endpoints, identities, and composition. -/
structure ProjectionFunctor
    {Point : Type u}
    (C D : _root_.CategoryTheory.Category.{v} Point) where
  map : {a b : Point} -> C.Plan a b -> D.Plan a b
  map_id : (a : Point) -> map (C.id a) = D.id a
  map_comp :
    {a b c : Point} -> (p : C.Plan a b) -> (q : C.Plan b c) ->
      map (p ≫ q) = (map p) ≫ (map q)

/--
The three endpoint-indexed plan categories and their two projections.

All three categories share the same endpoint type. The combined category is
the one later consumed by UK extraction.
-/
structure ReconciliationSystem where
  Point : Type u
  ICAO : FreeFinitePlanSystem.{u, v} Point
  ADEXP : FreeFinitePlanSystem.{u, v} Point
  Combined : FreeFinitePlanSystem.{u, v} Point
  toIcao : ProjectionFunctor Combined.cat ICAO.cat
  toAdexp : ProjectionFunctor Combined.cat ADEXP.cat

namespace ReconciliationSystem

abbrev IcaoPlan
    (R : ReconciliationSystem.{u, v})
    (a b : R.Point) : Type v :=
  R.ICAO.Plan a b

abbrev AdexpPlan
    (R : ReconciliationSystem.{u, v})
    (a b : R.Point) : Type v :=
  R.ADEXP.Plan a b

abbrev CombinedPlan
    (R : ReconciliationSystem.{u, v})
    (a b : R.Point) : Type v :=
  R.Combined.Plan a b

/--
A combined plan reconciles an ICAO plan and an ADEXP plan when its projections
are exactly those plans.
-/
abbrev Reconciles
    (R : ReconciliationSystem.{u, v})
    {a b : R.Point}
    (combi : R.CombinedPlan a b)
    (icao : R.IcaoPlan a b)
    (adexp : R.AdexpPlan a b) : Prop :=
  R.toIcao.map combi = icao ∧
  R.toAdexp.map combi = adexp

/--
Two adjacent reconciliations compose to a reconciliation of the composed
ICAO/ADEXP plans.
-/
theorem comp_reconciles
    (R : ReconciliationSystem.{u, v})
    {a b c : R.Point}
    {icao₁ : R.IcaoPlan a b}
    {icao₂ : R.IcaoPlan b c}
    {adexp₁ : R.AdexpPlan a b}
    {adexp₂ : R.AdexpPlan b c}
    {combi₁ : R.CombinedPlan a b}
    {combi₂ : R.CombinedPlan b c}
    (combi₁_reconciles : R.Reconciles combi₁ icao₁ adexp₁)
    (combi₂_reconciles : R.Reconciles combi₂ icao₂ adexp₂) :
    R.Reconciles (combi₁ ≫ combi₂) (icao₁ ≫ icao₂) (adexp₁ ≫ adexp₂) := by
  constructor
  · calc R.toIcao.map (combi₁ ≫ combi₂)
      _ = R.toIcao.map combi₁ ≫ R.toIcao.map combi₂   := by rw [R.toIcao.map_comp]
      _ = icao₁ ≫ icao₂                               := by rw [combi₁_reconciles.1, combi₂_reconciles.1]
  · calc R.toAdexp.map (combi₁ ≫ combi₂)
      _ = R.toAdexp.map combi₁ ≫ R.toAdexp.map combi₂ := by rw [R.toAdexp.map_comp]
      _ = adexp₁ ≫ adexp₂                             := by rw [combi₁_reconciles.2, combi₂_reconciles.2]

end ReconciliationSystem

abbrev LegPlan
    {Point : Type u}
    (S : FreeFinitePlanSystem.{u, v} Point)
    {a b : Point} : Type v :=
  { p : S.Plan a b // S.IsLeg p }

/--
One ICAO leg reconciles with one ADEXP portion through a unique combined leg.
-/
structure LegReconciliation
    (R : ReconciliationSystem.{u, v})
    {a b : R.Point}
    (icaoLeg : LegPlan R.ICAO (a := a) (b := b))
    (adexpPortion : R.AdexpPlan a b) where
  combinedLeg : LegPlan R.Combined (a := a) (b := b)
  toIcao_eq : R.toIcao.map combinedLeg.val = icaoLeg.val
  toAdexp_eq : R.toAdexp.map combinedLeg.val = adexpPortion
  unique :
    (other : LegPlan R.Combined (a := a) (b := b)) ->
      R.toIcao.map other.val = icaoLeg.val ->
      R.toAdexp.map other.val = adexpPortion ->
        other.val = combinedLeg.val

instance
    {R : ReconciliationSystem.{u, v}}
    {a b : R.Point}
    {icaoLeg : LegPlan R.ICAO (a := a) (b := b)}
    {adexp : R.AdexpPlan a b} :
    Subsingleton (LegReconciliation R icaoLeg adexp) :=
  ⟨by
    intro left right
    have hleg : right.combinedLeg = left.combinedLeg :=
      Subtype.ext <| left.unique right.combinedLeg right.toIcao_eq right.toAdexp_eq
    cases left
    cases right
    cases hleg
    simp⟩

/-- A finite left ADEXP factor consumed by the exposed ICAO leg. -/
structure FrontSplit
    (R : ReconciliationSystem.{u, v})
    {a m b : R.Point}
    (icaoLeg : LegPlan R.ICAO (a := a) (b := m))
    (adexp : R.AdexpPlan a b) where
  adexpPrefix : R.AdexpPlan a m
  rest : R.AdexpPlan m b
  factor : adexp = adexpPrefix ≫ rest
  nontrivial : 0 < R.ADEXP.length adexpPrefix
  legReconciliation : LegReconciliation R icaoLeg adexpPrefix

@[ext]
theorem FrontSplit.ext
    {R : ReconciliationSystem.{u, v}}
    {a m b : R.Point}
    {icaoLeg : LegPlan R.ICAO (a := a) (b := m)}
    {adexp : R.AdexpPlan a b}
    {left right : FrontSplit R icaoLeg adexp}
    (hprefix : left.adexpPrefix = right.adexpPrefix)
    (hrest : left.rest = right.rest) :
    left = right := by
  cases left
  cases right
  simp only at hprefix hrest
  cases hprefix
  cases hrest
  simp only [FrontSplit.mk.injEq, true_and]
  exact heq_of_eq (Subsingleton.elim _ _)

/-- Exact finite enumeration contract for front splits. -/
structure FrontSplitEnumeration
    (R : ReconciliationSystem.{u, v})
    {a m b : R.Point}
    (icaoLeg : LegPlan R.ICAO (a := a) (b := m))
    (adexp : R.AdexpPlan a b) where
  splits : List (FrontSplit R icaoLeg adexp)
  sound :
    ∀ s, s ∈ splits ->
      adexp = s.adexpPrefix ≫ s.rest ∧
      0 < R.ADEXP.length s.adexpPrefix ∧
      Nonempty (LegReconciliation R icaoLeg s.adexpPrefix)
  complete :
    ∀ (adexpPrefix : R.AdexpPlan a m) (rest : R.AdexpPlan m b),
      adexp = adexpPrefix ≫ rest ->
      0 < R.ADEXP.length adexpPrefix ->
      Nonempty (LegReconciliation R icaoLeg adexpPrefix) ->
        ∃ s ∈ splits, s.adexpPrefix = adexpPrefix ∧ s.rest = rest
  nodup : splits.Nodup

/--
An exact finite enumeration of the reconciliation fibre for one ICAO/ADEXP
pair.

This is the representation-independent contract for the concrete raw entry
point: enumerate every combined plan whose projections are the requested ICAO
and ADEXP plans, with no duplicate combined plans.
-/
structure ReconciliationFibreEnumeration
    (R : ReconciliationSystem.{u, v})
    {a b : R.Point}
    (icao : R.IcaoPlan a b)
    (adexp : R.AdexpPlan a b) where
  candidates : List (R.CombinedPlan a b)
  sound :
    ∀ c, c ∈ candidates ->
      R.Reconciles c icao adexp
  complete :
    ∀ c,
      R.toIcao.map c = icao ->
      R.toAdexp.map c = adexp ->
        c ∈ candidates
  nodup : candidates.Nodup

end Reconciliation

end Formal.Categorical
