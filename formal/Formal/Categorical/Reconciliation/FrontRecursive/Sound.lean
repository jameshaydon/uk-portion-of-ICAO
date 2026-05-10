import Formal.Categorical.Reconciliation.FrontRecursive.Base

namespace Formal.Categorical

universe u v

namespace Reconciliation
namespace FrontRecursive

variable {R : ReconciliationSystem.{u, v}}

/-- Every recursively enumerated candidate projects to the requested ICAO/ADEXP pair. -/
theorem candidates_sound
    (base : IdentityBaseEnumerator R)
    (frontSplits : FrontSplitEnumerator R)
    (laws : FrontRecursiveReconciliationLaws R)
    {a b : R.Point}
    (icao : R.IcaoPlan a b)
    (adexp : R.AdexpPlan a b)
    (combined : R.CombinedPlan a b)
    (hmem : combined ∈ candidates base frontSplits laws icao adexp) :
    R.Reconciles combined icao adexp := by
  revert combined
  induction a, b, icao, adexp using candidates.induct frontSplits with
  | case1 a b icao adexp hfront =>
      intro combined hmem
      have hicao_id :
          IsIdPlan icao :=
        (R.ICAO.frontDecomposition?_eq_none_iff_identity icao).1 hfront
      rw [candidates.eq_def] at hmem
      dsimp only at hmem
      rw [hfront] at hmem
      exact base.sound icao hicao_id adexp combined hmem
  | case2 a b icao adexp front hfront icaoLeg ih =>
      intro combined hmem
      let splitEnumeration := frontSplits.enumerate icaoLeg adexp
      rw [candidates_unfold_some base frontSplits laws icao adexp front hfront] at hmem
      simp only [List.mem_flatMap, List.mem_map] at hmem
      rcases hmem with ⟨split, hsplit_mem, rest, hrest_mem, hcombined⟩
      let legRec := split.legReconciliation
      have hrest_sound :
          R.Reconciles rest front.rest split.rest :=
        ih split rest hrest_mem
      have hcomposed :=
        ReconciliationSystem.comp_reconciles R
          ⟨legRec.toIcao_eq, legRec.toAdexp_eq⟩
          hrest_sound
      constructor
      · rw [← hcombined, hcomposed.1, front.factor]
      · rw [← hcombined, hcomposed.2, ← split.factor]

end FrontRecursive
end Reconciliation

end Formal.Categorical
