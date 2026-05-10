import Formal.Categorical.Reconciliation.FrontRecursive.Base

namespace Formal.Categorical

universe u v

namespace Reconciliation
namespace FrontRecursive

variable {R : ReconciliationSystem.{u, v}}

/-- Every combined plan projecting to the requested ICAO/ADEXP pair is enumerated. -/
theorem candidates_complete
    (base : IdentityBaseEnumerator R)
    (frontSplits : FrontSplitEnumerator R)
    (laws : FrontRecursiveReconciliationLaws R)
    {a b : R.Point}
    (icao : R.IcaoPlan a b)
    (adexp : R.AdexpPlan a b)
    (combined : R.CombinedPlan a b)
    (htoIcao : R.toIcao.map combined = icao)
    (htoAdexp : R.toAdexp.map combined = adexp) :
    combined ∈ candidates base frontSplits laws icao adexp := by
  revert combined
  induction a, b, icao, adexp using candidates.induct frontSplits with
  | case1 a b icao adexp hfront =>
      intro combined htoIcao htoAdexp
      have hicao_id :
          IsIdPlan icao :=
        (R.ICAO.frontDecomposition?_eq_none_iff_identity icao).1 hfront
      rw [candidates.eq_def]
      dsimp only
      rw [hfront]
      exact base.complete icao hicao_id adexp combined htoIcao htoAdexp
  | case2 a b icao adexp front hfront icaoLeg ih =>
      intro combined htoIcao htoAdexp
      let factorisation :=
        laws.front_complete icaoLeg front.rest adexp combined
          (by
            rw [front.factor]
            exact htoIcao)
          htoAdexp
      let splitEnumeration := frontSplits.enumerate icaoLeg adexp
      obtain ⟨split, hsplit_mem, hprefix_eq, hrest_eq⟩ :=
        splitEnumeration.complete
          factorisation.adexpPrefix
          factorisation.adexpRest
          factorisation.adexp_factor
          factorisation.adexpPrefix_nontrivial
          ⟨factorisation.legReconciliation⟩
      let legRec := split.legReconciliation
      have hleg_eq :
          factorisation.legReconciliation.combinedLeg.val =
            legRec.combinedLeg.val :=
        legRec.unique factorisation.legReconciliation.combinedLeg
          factorisation.legReconciliation.toIcao_eq
          (factorisation.legReconciliation.toAdexp_eq.trans hprefix_eq.symm)
      have hrest_mem :
          factorisation.combinedRest ∈
            candidates base frontSplits laws front.rest split.rest :=
        ih split factorisation.combinedRest
          factorisation.rest_toIcao_eq
          (factorisation.rest_toAdexp_eq.trans hrest_eq.symm)
      rw [candidates_unfold_some base frontSplits laws icao adexp front hfront]
      simp only [List.mem_flatMap, List.mem_map]
      refine ⟨split, hsplit_mem, factorisation.combinedRest, hrest_mem, ?_⟩
      simpa [hleg_eq] using factorisation.combined_factor.symm

end FrontRecursive
end Reconciliation

end Formal.Categorical
