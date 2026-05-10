import Formal.Categorical.Extraction

namespace Formal.Categorical

universe u v w

namespace Extraction

/-- The exposed first-leg boundary as an atomic occurrence. -/
def frontBoundaryOccurrence
    {Point : Type u}
    (S : FreeFinitePlanSystem.{u, v} Point)
    {a b : Point}
    {whole : S.Plan a b}
    (current : Subplan S.cat whole)
    (front : FrontDecomposition S.cat S.IsLeg current.middle) :
    AtomicOccurrence S whole where
  subplan := frontHead current front
  isAtomic := Or.inr front.first_isLeg

/-- The exposed last-leg boundary as an atomic occurrence. -/
def backBoundaryOccurrence
    {Point : Type u}
    (S : FreeFinitePlanSystem.{u, v} Point)
    {a b : Point}
    {whole : S.Plan a b}
    (current : Subplan S.cat whole)
    (back : BackDecomposition S.cat S.IsLeg current.middle) :
    AtomicOccurrence S whole where
  subplan := backBoundaryCandidate current back
  isAtomic := Or.inr back.last_isLeg

/-- An identity subplan as a point occurrence. -/
def identityOccurrence
    {Point : Type u}
    (S : FreeFinitePlanSystem.{u, v} Point)
    {a b : Point}
    {whole : S.Plan a b}
    (current : Subplan S.cat whole)
    (hidentity : IsIdPlan current.middle) :
    AtomicOccurrence S whole where
  subplan := current
  isAtomic := Or.inl hidentity

/-- One valid boundary-trimming step of the abstract shrinking algorithm. -/
structure BoundaryTrimStep
    {Point : Type u}
    (S : FreeFinitePlanSystem.{u, v} Point)
    (rel : UKRelevance S)
    {a b : Point}
    {whole : S.Plan a b}
    (current next : Subplan S.cat whole) : Prop where
  next_inside_current : next <= current
  proper : ¬ current <= next
  preserves_cover : CoversUK rel current -> CoversUK rel next
  length_decreases : S.length next.middle < S.length current.middle

/-- One iteration of the trimming implementation. -/
inductive BoundaryTrimDecision
    {Point : Type u}
    (S : FreeFinitePlanSystem.{u, v} Point)
    (rel : UKRelevance S)
    {a b : Point}
    {whole : S.Plan a b}
    (current : Subplan S.cat whole) : Type (max u v) where
  | trim :
      (next : Subplan S.cat whole) ->
      BoundaryTrimStep S rel current next ->
        BoundaryTrimDecision S rel current
  | done :
      CoversUK rel current ->
      LeastUKSubplan rel current ->
        BoundaryTrimDecision S rel current

/--
Try to chop the first leg from the current middle.

This is defined from `FreeFinitePlanSystem.frontDecomposition?`. The chop is
returned only when the exposed first-leg boundary occurrence is not UK.
-/
def frontChop?
    {Point : Type u}
    {S : FreeFinitePlanSystem.{u, v} Point}
    {rel : UKRelevance S}
    {a b : Point}
    {whole : S.Plan a b}
    (atomHasUKDecidable :
      (atom : AtomicOccurrence S whole) -> Decidable (AtomHasUK rel atom))
    (current : Subplan S.cat whole) :
    Option (Subplan S.cat whole) :=
  match S.frontDecomposition? current.middle with
  | none =>
      none
  | some front =>
      match atomHasUKDecidable (frontBoundaryOccurrence S current front) with
      | isTrue _ => none
      | isFalse _ => some (frontPeel current front)

/--
Try to chop the last leg from the current middle.

This is defined from `FreeFinitePlanSystem.backDecomposition?`. The chop is
returned only when the exposed last-leg boundary occurrence is not UK.
-/
def backChop?
    {Point : Type u}
    {S : FreeFinitePlanSystem.{u, v} Point}
    {rel : UKRelevance S}
    {a b : Point}
    {whole : S.Plan a b}
    (atomHasUKDecidable :
      (atom : AtomicOccurrence S whole) -> Decidable (AtomHasUK rel atom))
    (current : Subplan S.cat whole) :
    Option (Subplan S.cat whole) :=
  match S.backDecomposition? current.middle with
  | none =>
      none
  | some back =>
      match atomHasUKDecidable (backBoundaryOccurrence S current back) with
      | isTrue _ => none
      | isFalse _ => some (backInitCandidate current back)

/--
Correctness of a successful front chop: the exposed boundary was not UK, so the
front trim preserves coverage and decreases middle length.
-/
theorem front_chop_is_valid
    {Point : Type u}
    {S : FreeFinitePlanSystem.{u, v} Point}
    {rel : UKRelevance S}
    {a b : Point}
    {whole : S.Plan a b}
    (atomHasUKDecidable :
      (atom : AtomicOccurrence S whole) -> Decidable (AtomHasUK rel atom))
    (current next : Subplan S.cat whole)
    (hfront : frontChop? atomHasUKDecidable current = some next) :
    BoundaryTrimStep S rel current next := by
  unfold frontChop? at hfront
  cases hdecomp : S.frontDecomposition? current.middle with
  | none =>
      simp [hdecomp] at hfront
  | some front =>
      cases hboundary :
          atomHasUKDecidable (frontBoundaryOccurrence S current front) with
      | isTrue hboundary_uk =>
          simp [hdecomp, hboundary] at hfront
      | isFalse hboundary_not_uk =>
          simp [hdecomp, hboundary] at hfront
          subst next
          let next := frontPeel current front
          have hnext_le_current : next <= current := by
            refine ⟨{
              left := front.first
              right := S.cat.id current.j
              middle_factor := ?_
              pre_factor := ?_
              post_factor := ?_
            }⟩
            · change front.first ≫ front.rest ≫ S.cat.id current.j =
                current.middle
              simpa [Plan.comp] using front.factor
            · rfl
            · change S.cat.id current.j ≫ current.post = current.post
              simp [Plan.comp]
          have hlength_decreases :
              S.length next.middle < S.length current.middle := by
            exact S.front_rest_shorter front
          refine {
            next_inside_current := hnext_le_current
            proper := ?_
            preserves_cover := ?_
            length_decreases := hlength_decreases
          }
          · intro hcurrent_le_next
            have hsame : current = next :=
              FreeFinitePlanSystem.subplan_le_antisymm S
                hcurrent_le_next hnext_le_current
            have hlength_eq :
                S.length current.middle = S.length next.middle := by
              rw [hsame]
            omega
          · intro covers_current atom atom_uk
            have atom_le_current : atom.subplan <= current :=
              covers_current atom atom_uk
            rcases S.front_atom_le_boundary_or_remainder current front
                atom.subplan atom.isAtomic atom_le_current with
              atom_le_boundary | atom_le_remainder
            · have boundary_uk : AtomHasUK rel
                  (frontBoundaryOccurrence S current front) :=
                Or.inr ⟨front.first_isLeg,
                  rel.legHasUK_of_atom_le atom
                    ⟨frontHead current front, front.first_isLeg⟩
                    atom_uk atom_le_boundary⟩
              exact (hboundary_not_uk boundary_uk).elim
            · exact atom_le_remainder

/--
Correctness of a successful back chop: the exposed boundary was not UK, so the
back trim preserves coverage and decreases middle length.
-/
theorem back_chop_is_valid
    {Point : Type u}
    {S : FreeFinitePlanSystem.{u, v} Point}
    {rel : UKRelevance S}
    {a b : Point}
    {whole : S.Plan a b}
    (atomHasUKDecidable :
      (atom : AtomicOccurrence S whole) -> Decidable (AtomHasUK rel atom))
    (current next : Subplan S.cat whole)
    (hback : backChop? atomHasUKDecidable current = some next) :
    BoundaryTrimStep S rel current next := by
  unfold backChop? at hback
  cases hdecomp : S.backDecomposition? current.middle with
  | none =>
      simp [hdecomp] at hback
  | some back =>
      cases hboundary :
          atomHasUKDecidable (backBoundaryOccurrence S current back) with
      | isTrue hboundary_uk =>
          simp [hdecomp, hboundary] at hback
      | isFalse hboundary_not_uk =>
          simp [hdecomp, hboundary] at hback
          subst next
          let next := backInitCandidate current back
          have hnext_le_current : next <= current := by
            refine ⟨{
              left := S.cat.id current.i
              right := back.last
              middle_factor := ?_
              pre_factor := ?_
              post_factor := ?_
            }⟩
            · change S.cat.id current.i ≫ back.init ≫ back.last =
                current.middle
              simpa [Plan.comp] using back.factor
            · change current.pre ≫ S.cat.id current.i = current.pre
              simp [Plan.comp]
            · rfl
          have hlength_decreases :
              S.length next.middle < S.length current.middle := by
            exact S.back_init_shorter back
          refine {
            next_inside_current := hnext_le_current
            proper := ?_
            preserves_cover := ?_
            length_decreases := hlength_decreases
          }
          · intro hcurrent_le_next
            have hsame : current = next :=
              FreeFinitePlanSystem.subplan_le_antisymm S
                hcurrent_le_next hnext_le_current
            have hlength_eq :
                S.length current.middle = S.length next.middle := by
              rw [hsame]
            omega
          · intro covers_current atom atom_uk
            have atom_le_current : atom.subplan <= current :=
              covers_current atom atom_uk
            rcases S.back_atom_le_init_or_boundary current back
                atom.subplan atom.isAtomic atom_le_current with
              atom_le_init | atom_le_boundary
            · exact atom_le_init
            · have boundary_uk : AtomHasUK rel
                  (backBoundaryOccurrence S current back) :=
                Or.inr ⟨back.last_isLeg,
                  rel.legHasUK_of_atom_le atom
                    ⟨backBoundaryCandidate current back, back.last_isLeg⟩
                    atom_uk atom_le_boundary⟩
              exact (hboundary_not_uk boundary_uk).elim

/--
The middle of a located subplan is uniquely determined by its fixed outer
contexts inside the whole plan.
-/
theorem middle_unique
    {Point : Type u}
    {S : FreeFinitePlanSystem.{u, v} Point}
    {a b : Point}
    {whole : S.Plan a b}
    (s : Subplan S.cat whole)
    (m : S.Plan s.i s.j)
    (h : s.pre ≫ m ≫ s.post = whole) :
    m = s.middle := by
  have hcontext :
      s.pre ≫ (m ≫ s.post) =
        s.pre ≫ (s.middle ≫ s.post) := by
    simpa [CategoryTheory.Category.assoc] using h.trans s.factor.symm
  have hright : m ≫ s.post = s.middle ≫ s.post :=
    S.left_cancel s.pre hcontext
  exact S.right_cancel s.post hright

/--
If a target subplan contains both exposed boundary legs of `current`, then it
contains the whole current interval.
-/
theorem subplan_convex_boundaries
    {Point : Type u}
    {S : FreeFinitePlanSystem.{u, v} Point}
    {a b : Point}
    {whole : S.Plan a b}
    (current target : Subplan S.cat whole)
    (front : FrontDecomposition S.cat S.IsLeg current.middle)
    (back : BackDecomposition S.cat S.IsLeg current.middle)
    (hfront :
      frontHead current front <= target)
    (hback :
      backBoundaryCandidate current back <= target) :
    current <= target := by
  rcases hfront with ⟨frontData⟩
  rcases hback with ⟨backData⟩
  refine ⟨{
    left := frontData.left
    right := backData.right
    middle_factor := ?_
    pre_factor := frontData.pre_factor
    post_factor := backData.post_factor
  }⟩
  let middle : S.cat.Plan target.i target.j :=
    frontData.left ≫ current.middle ≫ backData.right
  have hmiddle : middle = target.middle := by
    apply middle_unique target middle
    have hpre : target.pre ≫ frontData.left = current.pre := by
      simpa [frontHead] using frontData.pre_factor
    have hpost : backData.right ≫ target.post = current.post := by
      simpa [backBoundaryCandidate] using backData.post_factor
    calc
      target.pre ≫ middle ≫ target.post =
          (target.pre ≫ frontData.left) ≫ current.middle ≫
            (backData.right ≫ target.post) := by
            simp [middle, CategoryTheory.Category.assoc]
      _ = current.pre ≫ current.middle ≫
            (backData.right ≫ target.post) := by
            exact congrArg
              (fun pre => pre ≫ current.middle ≫
                (backData.right ≫ target.post))
              hpre
      _ = current.pre ≫ current.middle ≫ current.post := by
            exact congrArg
              (fun post => current.pre ≫ current.middle ≫ post)
              hpost
      _ = whole := by
            exact current.factor
  simpa [middle] using hmiddle

/--
If neither executable boundary chop succeeds, the current subplan is least.
-/
theorem no_chop_is_least
    {Point : Type u}
    {S : FreeFinitePlanSystem.{u, v} Point}
    {rel : UKRelevance S}
    {a b : Point}
    {whole : S.Plan a b}
    (atomHasUKDecidable :
      (atom : AtomicOccurrence S whole) -> Decidable (AtomHasUK rel atom))
    (current : Subplan S.cat whole)
    (covers : CoversUK rel current)
    (hasUK : HasUK rel whole)
    (hfront : frontChop? atomHasUKDecidable current = none)
    (hback : backChop? atomHasUKDecidable current = none) :
    LeastUKSubplan rel current := by
  refine ⟨covers, ?_⟩
  intro target target_covers
  by_cases current_identity : IsIdPlan current.middle
  · rcases hasUK with ⟨atom, atom_uk⟩
    have atom_le_current : atom.subplan <= current := covers atom atom_uk
    have current_le_atom : current <= atom.subplan :=
      S.identity_subplan_le_of_le current_identity atom_le_current
    have current_eq_atom : current = atom.subplan :=
      FreeFinitePlanSystem.subplan_le_antisymm S
        current_le_atom atom_le_current
    have current_uk : AtomHasUK rel
        (identityOccurrence S current current_identity) := by
      subst current_eq_atom
      exact atom_uk
    exact target_covers (identityOccurrence S current current_identity)
      current_uk
  · cases hfront_decomp : S.frontDecomposition? current.middle with
    | none =>
        exact (current_identity
            ((S.frontDecomposition?_eq_none_iff_identity current.middle).1
              hfront_decomp)).elim
    | some front =>
        cases hback_decomp : S.backDecomposition? current.middle with
        | none =>
            exact (current_identity
                ((S.backDecomposition?_eq_none_iff_identity current.middle).1
                  hback_decomp)).elim
        | some back =>
            have front_uk : AtomHasUK rel
                (frontBoundaryOccurrence S current front) := by
              unfold frontChop? at hfront
              rw [hfront_decomp] at hfront
              cases hfront_boundary :
                  atomHasUKDecidable
                    (frontBoundaryOccurrence S current front) with
              | isTrue h =>
                  exact h
              | isFalse _ =>
                  simp [hfront_boundary] at hfront
            have back_uk : AtomHasUK rel
                (backBoundaryOccurrence S current back) := by
              unfold backChop? at hback
              rw [hback_decomp] at hback
              cases hback_boundary :
                  atomHasUKDecidable
                    (backBoundaryOccurrence S current back) with
              | isTrue h =>
                  exact h
              | isFalse _ =>
                  simp [hback_boundary] at hback
            exact subplan_convex_boundaries current target front back
              (target_covers
                (frontBoundaryOccurrence S current front) front_uk)
              (target_covers
                (backBoundaryOccurrence S current back) back_uk)

/--
One explicit trimming iteration: try the front, then try the back, otherwise
return the leastness certificate supplied by `no_chop_is_least`.
-/
def explicitTrimStep
    {Point : Type u}
    {S : FreeFinitePlanSystem.{u, v} Point}
    {rel : UKRelevance S}
    {a b : Point}
    {whole : S.Plan a b}
    (atomHasUKDecidable :
      (atom : AtomicOccurrence S whole) -> Decidable (AtomHasUK rel atom))
    (current : Subplan S.cat whole)
    (covers : CoversUK rel current)
    (hasUK : HasUK rel whole) :
    BoundaryTrimDecision S rel current :=
  match hfront : frontChop? atomHasUKDecidable current with
  | some front =>
      BoundaryTrimDecision.trim front
        (front_chop_is_valid atomHasUKDecidable current front hfront)
  | none =>
      match hback : backChop? atomHasUKDecidable current with
      | some back =>
          BoundaryTrimDecision.trim back
            (back_chop_is_valid atomHasUKDecidable current back hback)
      | none =>
          BoundaryTrimDecision.done
            covers
            (no_chop_is_least atomHasUKDecidable current covers hasUK hfront hback)

/--
Iterate explicit front/back chopping until no chop succeeds.

This is definitionally the algorithmic loop: run `explicitTrimStep`; recurse on
the returned smaller sub-plan after a chop; otherwise return the current
sub-plan, whose leastness is supplied by `no_chop_is_least`.
-/
def trimLoopByChopping
    {Point : Type u}
    {S : FreeFinitePlanSystem.{u, v} Point}
    {rel : UKRelevance S}
    {a b : Point}
    {whole : S.Plan a b}
    (atomHasUKDecidable :
      (atom : AtomicOccurrence S whole) -> Decidable (AtomHasUK rel atom))
    (current : Subplan S.cat whole)
    (covers : CoversUK rel current)
    (hasUK : HasUK rel whole) :
    { selected : Subplan S.cat whole //
      CoversUK rel selected ∧ LeastUKSubplan rel selected } :=
  match explicitTrimStep atomHasUKDecidable current covers hasUK with
  | BoundaryTrimDecision.done covers least =>
      ⟨current, covers, least⟩
  | BoundaryTrimDecision.trim next step =>
      trimLoopByChopping atomHasUKDecidable next (step.preserves_cover covers)
        hasUK
termination_by S.length current.middle
decreasing_by
  exact step.length_decreases

def trimFromWholeByChopping
    {Point : Type u}
    {S : FreeFinitePlanSystem.{u, v} Point}
    {rel : UKRelevance S}
    {a b : Point}
    (whole : S.Plan a b)
    (atomHasUKDecidable :
      (atom : AtomicOccurrence S whole) -> Decidable (AtomHasUK rel atom)) :
    HasUK rel whole ->
    { selected : Subplan S.cat whole //
      CoversUK rel selected ∧ LeastUKSubplan rel selected } :=
  fun hasUK =>
  trimLoopByChopping atomHasUKDecidable
    (Subplan.whole S.cat whole) (whole_covers rel) hasUK

end Extraction

end Formal.Categorical
