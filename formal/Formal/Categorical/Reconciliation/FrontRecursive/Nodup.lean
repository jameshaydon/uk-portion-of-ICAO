import Formal.Categorical.Reconciliation.FrontRecursive.Sound

namespace Formal.Categorical

universe u v

namespace Reconciliation
namespace FrontRecursive

variable {R : ReconciliationSystem.{u, v}}

private theorem nodup_flatMap_of_pairwise_disjoint
    {α : Type u}
    {β : Type v}
    {items : List α}
    {f : α -> List β}
    (hnodupItems : items.Nodup)
    (hnodupEach : ∀ item, item ∈ items -> (f item).Nodup)
    (hdisjoint :
      ∀ left, left ∈ items ->
      ∀ right, right ∈ items ->
      left ≠ right ->
      ∀ value, value ∈ f left -> value ∈ f right -> False) :
    (items.flatMap f).Nodup := by
  rw [List.nodup_flatMap]
  refine ⟨hnodupEach, ?_⟩
  rw [List.pairwise_iff_getElem]
  intro i j hi hj hij value hleft hright
  exact hdisjoint items[i] (List.getElem_mem hi)
    items[j] (List.getElem_mem hj)
    (by
      intro hsame
      have hidx := (hnodupItems.getElem_inj_iff).1 hsame
      omega)
    value hleft hright

/-- The recursive enumeration contains no duplicate combined plans. -/
theorem candidates_nodup
    (base : IdentityBaseEnumerator R)
    (frontSplits : FrontSplitEnumerator R)
    (laws : FrontRecursiveReconciliationLaws R)
    {a b : R.Point}
    (icao : R.IcaoPlan a b)
    (adexp : R.AdexpPlan a b) :
    (candidates base frontSplits laws icao adexp).Nodup := by
  induction a, b, icao, adexp using candidates.induct frontSplits with
  | case1 a b icao adexp hfront =>
      rw [candidates.eq_def]
      dsimp only
      rw [hfront]
      exact base.nodup adexp
  | case2 a b icao adexp front hfront icaoLeg ih =>
      rw [candidates_unfold_some base frontSplits laws icao adexp front hfront]
      let splitEnumeration := frontSplits.enumerate icaoLeg adexp
      refine nodup_flatMap_of_pairwise_disjoint splitEnumeration.nodup ?_ ?_
      · intro split hsplit_mem
        let legRec := split.legReconciliation
        exact (ih split).map fun leftRest rightRest hsame =>
          (laws.front_comp_no_confusion
            legRec.combinedLeg legRec.combinedLeg leftRest rightRest hsame).2
      · intro leftSplit hleft_mem rightSplit hright_mem hne value
          hvalue_left hvalue_right
        simp only [List.mem_map] at hvalue_left hvalue_right
        rcases hvalue_left with ⟨leftRest, hleftRest_mem, hleftValue_eq⟩
        rcases hvalue_right with ⟨rightRest, hrightRest_mem, hrightValue_eq⟩
        let leftRec := leftSplit.legReconciliation
        let rightRec := rightSplit.legReconciliation
        have hcomposed :
            leftRec.combinedLeg.val ≫ leftRest =
              rightRec.combinedLeg.val ≫ rightRest := by
          simpa [leftRec, rightRec] using hleftValue_eq.trans hrightValue_eq.symm
        have hparts :=
          laws.front_comp_no_confusion
            leftRec.combinedLeg rightRec.combinedLeg
            leftRest rightRest hcomposed
        have hprefix : leftSplit.adexpPrefix = rightSplit.adexpPrefix := by
          rw [← leftRec.toAdexp_eq, hparts.1, rightRec.toAdexp_eq]
        have hleft_sound :
            R.toAdexp.map leftRest = leftSplit.rest :=
          (candidates_sound base frontSplits laws front.rest leftSplit.rest
            leftRest hleftRest_mem).2
        have hright_sound :
            R.toAdexp.map rightRest = rightSplit.rest :=
          (candidates_sound base frontSplits laws front.rest rightSplit.rest
            rightRest hrightRest_mem).2
        have hrest : leftSplit.rest = rightSplit.rest := by
          rw [← hleft_sound, hparts.2, hright_sound]
        exact hne (by
          ext
          · exact hprefix
          · exact hrest)

end FrontRecursive
end Reconciliation

end Formal.Categorical
