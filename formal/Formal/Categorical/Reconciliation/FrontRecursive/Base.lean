import Formal.Categorical.Core.Proofs
import Formal.Categorical.Reconciliation

namespace Formal.Categorical

universe u v

namespace Reconciliation

/--
Executable source of exact front splits for one exposed ICAO leg against an
ADEXP plan.

This is deliberately separate from the reconciliation laws: concrete models
can enumerate candidate ADEXP prefixes however they like, as long as the
existing `FrontSplitEnumeration` contract is met.
-/
structure FrontSplitEnumerator
    (R : ReconciliationSystem.{u, v}) where
  enumerate :
    {a m b : R.Point} ->
      (icaoLeg : LegPlan R.ICAO (a := a) (b := m)) ->
      (adexp : R.AdexpPlan a b) ->
        FrontSplitEnumeration R icaoLeg adexp

/--
Executable source of base-case candidates.

When ICAO has no front leg, the recursive search delegates to this enumerator
instead of constructing the combined identity through a proof-only endpoint
equality. Concrete models can make this fully executable.
-/
structure IdentityBaseEnumerator
    (R : ReconciliationSystem.{u, v}) where
  candidates :
    {a b : R.Point} ->
      (adexp : R.AdexpPlan a b) ->
        List (R.CombinedPlan a b)
  sound :
    {a b : R.Point} ->
      (icao : R.IcaoPlan a b) ->
      IsIdPlan icao ->
      (adexp : R.AdexpPlan a b) ->
      (combined : R.CombinedPlan a b) ->
      combined ∈ candidates adexp ->
        R.Reconciles combined icao adexp
  complete :
    {a b : R.Point} ->
      (icao : R.IcaoPlan a b) ->
      IsIdPlan icao ->
      (adexp : R.AdexpPlan a b) ->
      (combined : R.CombinedPlan a b) ->
      R.toIcao.map combined = icao ->
      R.toAdexp.map combined = adexp ->
        combined ∈ candidates adexp
  nodup :
    {a b : R.Point} ->
      (adexp : R.AdexpPlan a b) ->
        (candidates adexp).Nodup

/--
The result of factoring a whole-plan reconciliation across the first ICAO leg.

The fields say that a combined reconciliation of
`icaoLeg.val ≫ icaoRest` and `adexp` can be written as a combined leg followed
by a remaining combined plan, and that the ADEXP projection factors across the
same front split.
-/
structure FrontReconciliationFactorisation
    (R : ReconciliationSystem.{u, v})
    {a m b : R.Point}
    (icaoLeg : LegPlan R.ICAO (a := a) (b := m))
    (icaoRest : R.IcaoPlan m b)
    (adexp : R.AdexpPlan a b)
    (combined : R.CombinedPlan a b) where
  adexpPrefix : R.AdexpPlan a m
  adexpRest : R.AdexpPlan m b
  adexp_factor : adexp = adexpPrefix ≫ adexpRest
  adexpPrefix_nontrivial : 0 < R.ADEXP.length adexpPrefix
  legReconciliation : LegReconciliation R icaoLeg adexpPrefix
  combinedRest : R.CombinedPlan m b
  combined_factor :
    combined = legReconciliation.combinedLeg.val ≫ combinedRest
  rest_toIcao_eq : R.toIcao.map combinedRest = icaoRest
  rest_toAdexp_eq : R.toAdexp.map combinedRest = adexpRest

/--
Minimal reconciliation-specific laws needed to justify the front-recursive
enumerator without adding assumptions to `Core.lean`.

* `toIcao_reflects_identity` is the base-case no-silent-leg law: once ICAO has
  no front leg, any reconciliation projecting to it is itself an identity, so
  only identity ADEXP can match.
* `front_complete` is the one-step completeness/factorisation principle for a
  reconciled whole plan.
* `front_comp_no_confusion` prevents duplicate composed results in the step
  case: with the same exposed cut, a front combined leg followed by a rest is
  an injective presentation.
-/
structure FrontRecursiveReconciliationLaws
    (R : ReconciliationSystem.{u, v}) where
  toIcao_reflects_identity :
    {a b : R.Point} ->
      (combined : R.CombinedPlan a b) ->
      IsIdPlan (R.toIcao.map combined) ->
        IsIdPlan combined
  front_complete :
    {a m b : R.Point} ->
      (icaoLeg : LegPlan R.ICAO (a := a) (b := m)) ->
      (icaoRest : R.IcaoPlan m b) ->
      (adexp : R.AdexpPlan a b) ->
      (combined : R.CombinedPlan a b) ->
      R.toIcao.map combined = icaoLeg.val ≫ icaoRest ->
      R.toAdexp.map combined = adexp ->
        FrontReconciliationFactorisation R icaoLeg icaoRest adexp combined
  front_comp_no_confusion :
    {a m b : R.Point} ->
      (leftLeg rightLeg : LegPlan R.Combined (a := a) (b := m)) ->
      (leftRest rightRest : R.CombinedPlan m b) ->
      leftLeg.val ≫ leftRest = rightLeg.val ≫ rightRest ->
        leftLeg.val = rightLeg.val ∧ leftRest = rightRest

namespace FrontRecursive

variable {R : ReconciliationSystem.{u, v}}

/--
The raw front-recursive candidate list.

The recursion is over the ICAO plan.  In the step case it exposes the first
ICAO leg, enumerates every admissible ADEXP front split for that leg, recurses
on each ADEXP remainder, and composes the chosen unique combined leg with each
recursive combined rest.
-/
def candidates
    (base : IdentityBaseEnumerator R)
    (frontSplits : FrontSplitEnumerator R)
    (laws : FrontRecursiveReconciliationLaws R) :
    {a b : R.Point} ->
      (icao : R.IcaoPlan a b) ->
      (adexp : R.AdexpPlan a b) ->
        List (R.CombinedPlan a b)
  | a, b, icao, adexp =>
      match R.ICAO.frontDecomposition? icao with
      | none =>
          base.candidates adexp
      | some front =>
          let icaoLeg : LegPlan R.ICAO (a := a) (b := front.cut) :=
            ⟨front.first, front.first_isLeg⟩
          let splitEnumeration := frontSplits.enumerate icaoLeg adexp
          splitEnumeration.splits.flatMap fun split =>
            let legRec := split.legReconciliation
            (candidates base frontSplits laws front.rest split.rest).map fun rest =>
              legRec.combinedLeg.val ≫ rest
termination_by a b icao _adexp => R.ICAO.length icao
decreasing_by
  exact R.ICAO.front_rest_shorter front

/-- Unfold the recursive candidate enumerator at a nonempty ICAO front. -/
theorem candidates_unfold_some
    (base : IdentityBaseEnumerator R)
    (frontSplits : FrontSplitEnumerator R)
    (laws : FrontRecursiveReconciliationLaws R)
    {a b : R.Point}
    (icao : R.IcaoPlan a b)
    (adexp : R.AdexpPlan a b)
    (front : FrontDecomposition R.ICAO.cat R.ICAO.IsLeg icao)
    (hfront : R.ICAO.frontDecomposition? icao = some front) :
    candidates base frontSplits laws icao adexp =
      (frontSplits.enumerate
        (⟨front.first, front.first_isLeg⟩ : LegPlan R.ICAO (a := a) (b := front.cut))
        adexp).splits.flatMap fun split =>
          (candidates base frontSplits laws front.rest split.rest).map fun rest =>
            split.legReconciliation.combinedLeg.val ≫ rest := by
  rw [candidates.eq_def]
  dsimp only
  rw [hfront]

end FrontRecursive

end Reconciliation

end Formal.Categorical
