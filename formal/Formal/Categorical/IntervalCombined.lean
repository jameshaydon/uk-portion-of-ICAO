import Formal.Categorical.Concrete

namespace Formal.Categorical

open Extraction

universe u

namespace Model.IntervalCombined

/--
The interval category over a fixed plan with `n` legs.

Objects are boundary indexes `0 .. n`.  A morphism from `i` to `j` is the
interval `[i, j]`, represented by the proof that `i <= j`.
-/
structure Hom (n : Nat) (i j : Fin (n + 1)) where
  marker : Unit := ()
  le : i.val <= j.val

namespace Hom

@[ext] theorem ext {n : Nat} {i j : Fin (n + 1)}
    (p q : Hom n i j) : p = q := by
  cases p
  cases q
  congr

def id (n : Nat) (i : Fin (n + 1)) : Hom n i i :=
  ⟨(), Nat.le_refl i.val⟩

def comp {n : Nat} {i j k : Fin (n + 1)}
    (p : Hom n i j) (q : Hom n j k) : Hom n i k :=
  ⟨(), Nat.le_trans p.le q.le⟩

def length {n : Nat} {i j : Fin (n + 1)} (_p : Hom n i j) : Nat :=
  j.val - i.val

def isLeg {n : Nat} {i j : Fin (n + 1)} (_p : Hom n i j) : Prop :=
  j.val = i.val + 1

end Hom

@[reducible] def category (n : Nat) :
    CategoryTheory.Category (Fin (n + 1)) where
  Hom := Hom n
  id := Hom.id n
  comp := Hom.comp
  id_comp := by
    intro _ _ p
    exact Hom.ext _ _
  comp_id := by
    intro _ _ p
    exact Hom.ext _ _
  assoc := by
    intro _ _ _ _ _ _ _
    exact Hom.ext _ _

@[simp] theorem hom_ext_eq {n : Nat} {i j : Fin (n + 1)}
    (p q : Hom n i j) : p = q :=
  Hom.ext p q

theorem isIdPlan_iff_length_zero {n : Nat} {i j : Fin (n + 1)}
    (p : Hom n i j) :
    IsIdPlan (C := category n) p <-> Hom.length p = 0 := by
  constructor
  · intro h
    rcases h with ⟨hend, _⟩
    cases hend
    simp [Hom.length]
  · intro hlen
    have hji : j.val = i.val := by
      unfold Hom.length at hlen
      have hle := p.le
      omega
    have hend : j = i := Fin.ext hji
    refine ⟨hend, ?_⟩
    cases hend
    exact Hom.ext _ _

theorem comp_length {n : Nat} {i j k : Fin (n + 1)}
    (p : Hom n i j) (q : Hom n j k) :
    Hom.length (Hom.comp p q) = Hom.length p + Hom.length q := by
  unfold Hom.length
  have hij := p.le
  have hjk := q.le
  omega

theorem left_cancel {n : Nat} {i j k : Fin (n + 1)}
    (p : Hom n i j) {q r : Hom n j k}
    (_h : Hom.comp p q = Hom.comp p r) : q = r :=
  Hom.ext _ _

theorem right_cancel {n : Nat} {i j k : Fin (n + 1)}
    {p q : Hom n i j} (r : Hom n j k)
    (_h : Hom.comp p r = Hom.comp q r) : p = q :=
  Hom.ext _ _

theorem leg_length_positive {n : Nat} {i j : Fin (n + 1)}
    {p : Hom n i j} (h : Hom.isLeg p) : 0 < Hom.length p := by
  unfold Hom.length Hom.isLeg at *
  omega

private def succFin {n : Nat} (i : Fin (n + 1)) (h : i.val < n) :
    Fin (n + 1) :=
  ⟨i.val + 1, Nat.succ_lt_succ h⟩

private def predFin {n : Nat} (j : Fin (n + 1)) (h : 0 < j.val) :
    Fin (n + 1) :=
  ⟨j.val - 1, by omega⟩

def frontDecomposition? {n : Nat} :
    {i j : Fin (n + 1)} ->
    (p : Hom n i j) ->
    Option
      (FrontDecomposition
        (category n)
        (@Hom.isLeg n)
        p)
  | i, j, p =>
      if hlt : i.val < j.val then
        let cut : Fin (n + 1) :=
          succFin i (by omega)
        some
          { cut := cut
            first := ⟨(), by dsimp [cut, succFin]; omega⟩
            rest := ⟨(), by dsimp [cut, succFin]; omega⟩
            first_isLeg := by rfl
            factor := Hom.ext _ _ }
      else
        none

def backDecomposition? {n : Nat} :
    {i j : Fin (n + 1)} ->
    (p : Hom n i j) ->
    Option
      (BackDecomposition
        (category n)
        (@Hom.isLeg n)
        p)
  | i, j, p =>
      if hlt : i.val < j.val then
        let cut : Fin (n + 1) :=
          predFin j (by omega)
        some
          { cut := cut
            init := ⟨(), by dsimp [cut, predFin]; omega⟩
            last := ⟨(), by dsimp [cut, predFin]; omega⟩
            last_isLeg := by dsimp [Hom.isLeg, cut, predFin]; omega
            factor := Hom.ext _ _ }
      else
        none

theorem frontDecomposition?_none_iff_identity {n : Nat}
    {i j : Fin (n + 1)} (p : Hom n i j) :
    frontDecomposition? p = none <-> IsIdPlan (C := category n) p := by
  constructor
  · intro h
    unfold frontDecomposition? at h
    by_cases hlt : i.val < j.val
    · simp [hlt] at h
    · have hlen : Hom.length p = 0 := by
        unfold Hom.length
        omega
      exact (isIdPlan_iff_length_zero p).2 hlen
  · intro hid
    have hlen : Hom.length p = 0 :=
      (isIdPlan_iff_length_zero p).1 hid
    unfold frontDecomposition?
    have hnlt : ¬ i.val < j.val := by
      unfold Hom.length at hlen
      omega
    simp [hnlt]

theorem backDecomposition?_none_iff_identity {n : Nat}
    {i j : Fin (n + 1)} (p : Hom n i j) :
    backDecomposition? p = none <-> IsIdPlan (C := category n) p := by
  constructor
  · intro h
    unfold backDecomposition? at h
    by_cases hlt : i.val < j.val
    · simp [hlt] at h
    · have hlen : Hom.length p = 0 := by
        unfold Hom.length
        omega
      exact (isIdPlan_iff_length_zero p).2 hlen
  · intro hid
    have hlen : Hom.length p = 0 :=
      (isIdPlan_iff_length_zero p).1 hid
    unfold backDecomposition?
    have hnlt : ¬ i.val < j.val := by
      unfold Hom.length at hlen
      omega
    simp [hnlt]

private def le_of_bounds {n : Nat} {a b : Fin (n + 1)}
    (h : a.val <= b.val) : Hom n a b :=
  ⟨(), h⟩

private theorem subplan_le_of_bounds {n : Nat}
    {a b : Fin (n + 1)} {whole : Hom n a b}
    (small large : Subplan (category n) whole)
    (hli : large.i.val <= small.i.val)
    (hrj : small.j.val <= large.j.val) :
    small <= large := by
  refine ⟨{
    left := le_of_bounds hli
    right := le_of_bounds hrj
    middle_factor := Hom.ext _ _
    pre_factor := Hom.ext _ _
    post_factor := Hom.ext _ _
  }⟩

private theorem bounds_of_subplan_le {n : Nat}
    {a b : Fin (n + 1)} {whole : Hom n a b}
    {small large : Subplan (category n) whole}
    (h : small <= large) :
    large.i.val <= small.i.val ∧ small.j.val <= large.j.val := by
  rcases h with ⟨data⟩
  exact
    ⟨(show Hom n large.i small.i from data.left).le,
      (show Hom n small.j large.j from data.right).le⟩

theorem front_atom_le_boundary_or_remainder {n : Nat}
    {a b : Fin (n + 1)} {whole : Hom n a b}
    (current : Subplan (category n) whole)
    (front : FrontDecomposition (category n) (@Hom.isLeg n) current.middle)
    (atom : Subplan (category n) whole)
    (hatom : IsIdPlan atom.middle ∨ Hom.isLeg atom.middle)
    (hle : atom <= current) :
      atom <= frontHead current front ∨
        atom <= frontPeel current front := by
  have hbounds := bounds_of_subplan_le hle
  have hfront : front.cut.val = current.i.val + 1 := by
    exact front.first_isLeg
  have hmiddle : atom.j.val <= atom.i.val + 1 := by
    rcases hatom with hid | hleg
    · have hlen : Hom.length atom.middle = 0 :=
        (isIdPlan_iff_length_zero atom.middle).1 hid
      unfold Hom.length at hlen
      omega
    · unfold Hom.isLeg at hleg
      omega
  rcases Nat.eq_or_lt_of_le hbounds.1 with hi_eq | hi_lt
  · left
    apply subplan_le_of_bounds
    · simp [frontHead]
      omega
    · simp [frontHead]
      omega
  · right
    apply subplan_le_of_bounds
    · simp [frontPeel]
      omega
    · simp [frontPeel]
      exact hbounds.2

theorem back_atom_le_init_or_boundary {n : Nat}
    {a b : Fin (n + 1)} {whole : Hom n a b}
    (current : Subplan (category n) whole)
    (back : BackDecomposition (category n) (@Hom.isLeg n) current.middle)
    (atom : Subplan (category n) whole)
    (hatom : IsIdPlan atom.middle ∨ Hom.isLeg atom.middle)
    (hle : atom <= current) :
      atom <= backInitCandidate current back ∨
        atom <= backBoundaryCandidate current back := by
  have hbounds := bounds_of_subplan_le hle
  have hback : current.j.val = back.cut.val + 1 := by
    exact back.last_isLeg
  have hmiddle : atom.j.val <= atom.i.val + 1 := by
    rcases hatom with hid | hleg
    · have hlen : Hom.length atom.middle = 0 :=
        (isIdPlan_iff_length_zero atom.middle).1 hid
      unfold Hom.length at hlen
      omega
    · unfold Hom.isLeg at hleg
      omega
  by_cases hj_le : atom.j.val <= back.cut.val
  · left
    apply subplan_le_of_bounds
    · simp [backInitCandidate]
      exact hbounds.1
    · simp [backInitCandidate]
      exact hj_le
  · right
    apply subplan_le_of_bounds
    · simp [backBoundaryCandidate]
      omega
    · simp [backBoundaryCandidate]
      omega

def system (n : Nat) : FreeFinitePlanSystem (Fin (n + 1)) where
  cat := category n
  IsLeg := @Hom.isLeg n
  length := @Hom.length n
  id_iff_length_zero := @isIdPlan_iff_length_zero n
  comp_length := @comp_length n
  left_cancel := @left_cancel n
  right_cancel := @right_cancel n
  leg_length_positive := @leg_length_positive n
  frontDecomposition? := @frontDecomposition? n
  backDecomposition? := @backDecomposition? n
  frontDecomposition?_eq_none_iff_identity :=
    @frontDecomposition?_none_iff_identity n
  backDecomposition?_eq_none_iff_identity :=
    @backDecomposition?_none_iff_identity n
  front_atom_le_boundary_or_remainder :=
    @front_atom_le_boundary_or_remainder n
  back_atom_le_init_or_boundary :=
    @back_atom_le_init_or_boundary n

/-- Route data for one concrete combined leg, detached from index endpoints. -/
structure AnyVia
    (Point : Type u)
    (Route : Point -> Point -> Type u) where
  start : Point
  finish : Point
  route : Route start finish
  through : List Point

/--
Array-backed payload for a concrete combined plan.

The interval category only stores positions.  This payload stores the data
needed to interpret point and leg occurrences in the original waypoint domain.
-/
structure PlanData
    (Point : Type u)
    (Route : Point -> Point -> Type u)
    (n : Nat) where
  points : Fin (n + 1) -> Point
  legs : Fin n -> AnyVia Point Route

namespace PlanData

def fromCombined
    {Point : Type u}
    {Route : Point -> Point -> Type u} :
    {a b : Point} ->
      Model.Concrete.Combined Point Route a b ->
        Σ n : Nat, PlanData Point Route n
  | point, _, Model.Plan.nil _ =>
      ⟨0,
        { points := fun _ => point
          legs := fun i => nomatch i }⟩
  | start, finish, Model.Plan.cons (mid := mid) via rest =>
      let tail := fromCombined rest
      ⟨tail.1 + 1,
        { points := fun
            | ⟨0, _⟩ => start
            | ⟨Nat.succ k, hk⟩ =>
                tail.2.points ⟨k, by omega⟩
          legs := fun
            | ⟨0, _⟩ =>
                { start := start
                  finish := mid
                  route := via.route
                  through := via.through }
            | ⟨Nat.succ k, hk⟩ =>
                tail.2.legs ⟨k, by omega⟩ }⟩

def whole {Point : Type u} {Route : Point -> Point -> Type u}
    {n : Nat} (_data : PlanData Point Route n) :
    (system n).Plan ⟨0, Nat.succ_pos n⟩ ⟨n, Nat.lt_succ_self n⟩ :=
  ⟨(), Nat.zero_le n⟩

end PlanData

namespace UK

variable {Point : Type u}
variable {Route : Point -> Point -> Type u}
variable {n : Nat}

def legIndexHasUK (data : PlanData Point Route n) (uk : Point -> Prop)
    (i : Fin n) : Prop :=
  uk (data.points ⟨i.val, by omega⟩) ∨
    (∃ p : Point, p ∈ (data.legs i).through ∧ uk p) ∨
      uk (data.points ⟨i.val + 1, by omega⟩)

def pointHasUK (data : PlanData Point Route n) (uk : Point -> Prop)
    {a b : Fin (n + 1)}
    {whole : (system n).Plan a b}
    (point : PointOccurrence (system n) whole) : Prop :=
  uk (data.points point.val.i)

def legHasUK (data : PlanData Point Route n) (uk : Point -> Prop)
    {a b : Fin (n + 1)}
    {whole : (system n).Plan a b}
    (leg : LegOccurrence (system n) whole) : Prop :=
  ∃ h : leg.val.j.val = leg.val.i.val + 1,
    legIndexHasUK data uk
      ⟨leg.val.i.val, by
        have hj_bound := leg.val.j.isLt
        omega⟩

def relevance (data : PlanData Point Route n) (uk : Point -> Prop) :
    UKRelevance (system n) where
  pointHasUK := pointHasUK data uk
  legHasUK := legHasUK data uk
  legHasUK_of_atom_le := by
    intro a b whole atom leg atom_uk hle
    rcases leg.property with hleg
    change leg.val.j.val = leg.val.i.val + 1 at hleg
    refine ⟨hleg, ?_⟩
    rcases atom_uk with ⟨hpoint_atomic, hpoint_uk⟩ | ⟨hatom_leg, hatom_uk⟩
    · rcases bounds_of_subplan_le hle with ⟨hi, hj⟩
      unfold pointHasUK at hpoint_uk
      unfold legIndexHasUK
      have hpoint_len :
          atom.subplan.j.val = atom.subplan.i.val := by
        have hlen : Hom.length atom.subplan.middle = 0 :=
          (isIdPlan_iff_length_zero atom.subplan.middle).1 hpoint_atomic
        unfold Hom.length at hlen
        have hmid_le :=
          (show Hom n atom.subplan.i atom.subplan.j from
            atom.subplan.middle).le
        omega
      have hleft_or_right :
          atom.subplan.i.val = leg.val.i.val ∨
            atom.subplan.i.val = leg.val.j.val := by
        omega
      rcases hleft_or_right with hleft | hright
      · left
        have hidx : atom.subplan.i = leg.val.i := Fin.ext hleft
        simpa [hidx] using hpoint_uk
      · right
        right
        have hidx : atom.subplan.i = leg.val.j := Fin.ext hright
        have hjidx :
            leg.val.j =
              ⟨leg.val.i.val + 1, by
                rw [← hleg]
                exact leg.val.j.isLt⟩ := by
          exact Fin.ext hleg
        simpa [hidx, hjidx] using hpoint_uk
    · rcases bounds_of_subplan_le hle with ⟨hi, hj⟩
      unfold legHasUK at hatom_uk
      rcases hatom_uk with ⟨hatom_len, hatom_index_uk⟩
      change atom.subplan.j.val = atom.subplan.i.val + 1 at hatom_len
      have hsame : atom.subplan.i.val = leg.val.i.val := by
        omega
      have hidx :
          (⟨atom.subplan.i.val, by
            rw [hsame]
            have hbound := leg.val.j.isLt
            omega⟩ : Fin n) =
          (⟨leg.val.i.val, by
            have hbound := leg.val.j.isLt
            omega⟩ : Fin n) := by
        exact Fin.ext hsame
      simpa [hidx] using hatom_index_uk

def throughHasUKDecidable
    (uk : Point -> Prop)
    [DecidablePred uk] :
    (through : List Point) ->
      Decidable (∃ p : Point, p ∈ through ∧ uk p)
  | [] =>
      isFalse (by aesop)
  | point :: rest =>
      match inferInstanceAs (Decidable (uk point)),
        throughHasUKDecidable uk rest with
      | isTrue hpoint, _ =>
          isTrue ⟨point, List.Mem.head _, hpoint⟩
      | isFalse _hpoint, isTrue hrest =>
          isTrue (by
            rcases hrest with ⟨p, hp, huk⟩
            exact ⟨p, List.Mem.tail _ hp, huk⟩)
      | isFalse hpoint, isFalse hrest =>
          isFalse (by
            intro h
            rcases h with ⟨p, hp, huk⟩
            cases hp with
            | head => exact hpoint huk
            | tail _ hp => exact hrest ⟨p, hp, huk⟩)

def legIndexHasUKDecidable
    (data : PlanData Point Route n)
    (uk : Point -> Prop)
    [DecidablePred uk]
    (i : Fin n) :
    Decidable (legIndexHasUK data uk i) :=
  match inferInstanceAs
      (Decidable (uk (data.points ⟨i.val, by omega⟩))),
    throughHasUKDecidable uk (data.legs i).through,
    inferInstanceAs
      (Decidable (uk (data.points ⟨i.val + 1, by omega⟩))) with
  | isTrue hstart, _, _ =>
      isTrue (Or.inl hstart)
  | isFalse _hstart, isTrue hthrough, _ =>
      isTrue (Or.inr (Or.inl hthrough))
  | isFalse _hstart, isFalse _hthrough, isTrue hfinish =>
      isTrue (Or.inr (Or.inr hfinish))
  | isFalse hstart, isFalse hthrough, isFalse hfinish =>
      isFalse (by
        intro h
        rcases h with h | h | h
        · exact hstart h
        · exact hthrough h
        · exact hfinish h)

def atomHasUKDecidable
    (data : PlanData Point Route n)
    (uk : Point -> Prop)
    [DecidablePred uk]
    {a b : Fin (n + 1)}
    {whole : (system n).Plan a b}
    (atom : AtomicOccurrence (system n) whole) :
    Decidable (AtomHasUK (relevance data uk) atom) := by
  rcases atom with ⟨subplan, hatomic⟩
  by_cases hzero : Hom.length subplan.middle = 0
  · have hpoint : IsPointSubplan (system n) subplan :=
      (isIdPlan_iff_length_zero subplan.middle).2 hzero
    cases (inferInstance : Decidable (uk (data.points subplan.i))) with
    | isTrue h =>
        exact isTrue (Or.inl ⟨hpoint, h⟩)
    | isFalse hnot =>
        refine isFalse ?_
        intro h
        rcases h with ⟨_hpoint, hpoint_uk⟩ | ⟨hleg, _hleg_uk⟩
        · exact hnot hpoint_uk
        · have hpos : 0 < Hom.length subplan.middle :=
            leg_length_positive hleg
          omega
  · have hleg : IsLegSubplan (system n) subplan := by
      rcases hatomic with hpoint | hleg
      · have hlen : Hom.length subplan.middle = 0 :=
          (isIdPlan_iff_length_zero subplan.middle).1 hpoint
        exact False.elim (hzero hlen)
      · exact hleg
    have hleg_eq : subplan.j.val = subplan.i.val + 1 := hleg
    let index : Fin n :=
      ⟨subplan.i.val, by
        have hj_bound := subplan.j.isLt
        omega⟩
    cases legIndexHasUKDecidable data uk index with
    | isTrue hindex =>
        exact isTrue (Or.inr ⟨hleg, hleg_eq, by simpa [index] using hindex⟩)
    | isFalse hindex_not =>
        refine isFalse ?_
        intro h
        rcases h with ⟨hpoint, _hpoint_uk⟩ | ⟨_hleg, hleg_uk⟩
        · have hlen : Hom.length subplan.middle = 0 :=
            (isIdPlan_iff_length_zero subplan.middle).1 hpoint
          exact hzero hlen
        · rcases hleg_uk with ⟨_hleg_eq, hindex_uk⟩
          have hidx :
              (⟨subplan.i.val, by
                have hj_bound := subplan.j.isLt
                omega⟩ : Fin n) = index := rfl
          exact hindex_not (by simpa [index, hidx] using hindex_uk)

def legAtomicOccurrence
    (data : PlanData Point Route n)
    (i : Fin n) :
    AtomicOccurrence (system n) (PlanData.whole data) where
  subplan :=
    { i := ⟨i.val, by omega⟩
      j := ⟨i.val + 1, by
        have hi := i.isLt
        omega⟩
      pre := ⟨(), Nat.zero_le i.val⟩
      middle := ⟨(), Nat.le_succ i.val⟩
      post := ⟨(), by
        exact Nat.succ_le_of_lt i.isLt⟩
      factor := Hom.ext _ _ }
  isAtomic := Or.inr (by rfl)

theorem hasUK_of_legIndexHasUK
    (data : PlanData Point Route n)
    (uk : Point -> Prop)
    (i : Fin n)
    (h : legIndexHasUK data uk i) :
    HasUK (relevance data uk) (PlanData.whole data) := by
  refine ⟨legAtomicOccurrence data i, ?_⟩
  right
  refine ⟨by rfl, rfl, ?_⟩
  simpa [legAtomicOccurrence] using h

end UK

def extractIndexes
    {Point : Type u}
    {Route : Point -> Point -> Type u}
    {n : Nat}
    (data : PlanData Point Route n)
    (uk : Point -> Prop)
    [DecidablePred uk]
    (hasUK : HasUK (UK.relevance data uk) (PlanData.whole data)) :
    Nat × Nat :=
  let result :=
    trimFromWholeByChopping
      (S := system n)
      (rel := UK.relevance data uk)
      (PlanData.whole data)
      (UK.atomHasUKDecidable data uk)
      hasUK
  (result.val.i.val, result.val.j.val)

namespace Examples

open Model.Concrete.Examples

def convertedExample :
    Σ n : Nat, PlanData Nat ExampleRoute n :=
  PlanData.fromCombined examplePlan

def convertedExampleData : PlanData Nat ExampleRoute 5 :=
  convertedExample.2

theorem convertedExampleHasUK :
    HasUK
      (UK.relevance convertedExampleData exampleUK)
      (PlanData.whole convertedExampleData) := by
  apply UK.hasUK_of_legIndexHasUK convertedExampleData exampleUK
    (⟨1, by decide⟩ : Fin 5)
  unfold UK.legIndexHasUK
  right
  left
  exact
    ⟨20, by native_decide, by
      unfold exampleUK
      exact Or.inl rfl⟩

#eval extractIndexes convertedExampleData exampleUK convertedExampleHasUK

def widerExamplePlan : Model.Concrete.Combined Nat ExampleRoute 0 10 :=
  Model.Plan.cons ({ route := (), through := [] } :
      Model.Concrete.Via Nat ExampleRoute 0 1)
    (Model.Plan.cons ({ route := (), through := [] } :
        Model.Concrete.Via Nat ExampleRoute 1 2)
      (Model.Plan.cons ({ route := (), through := [] } :
          Model.Concrete.Via Nat ExampleRoute 2 3)
        (Model.Plan.cons ({ route := (), through := [] } :
            Model.Concrete.Via Nat ExampleRoute 3 4)
          (Model.Plan.cons ({ route := (), through := [20] } :
              Model.Concrete.Via Nat ExampleRoute 4 5)
            (Model.Plan.cons ({ route := (), through := [] } :
                Model.Concrete.Via Nat ExampleRoute 5 6)
              (Model.Plan.cons ({ route := (), through := [] } :
                  Model.Concrete.Via Nat ExampleRoute 6 7)
                (Model.Plan.cons ({ route := (), through := [40] } :
                    Model.Concrete.Via Nat ExampleRoute 7 8)
                  (Model.Plan.cons ({ route := (), through := [] } :
                      Model.Concrete.Via Nat ExampleRoute 8 9)
                    (Model.Plan.cons ({ route := (), through := [] } :
                        Model.Concrete.Via Nat ExampleRoute 9 10)
                      (Model.Plan.nil 10))))))))))

def convertedWiderExample :
    Σ n : Nat, PlanData Nat ExampleRoute n :=
  PlanData.fromCombined widerExamplePlan

def convertedWiderExampleData : PlanData Nat ExampleRoute 10 :=
  convertedWiderExample.2

theorem convertedWiderExampleHasUK :
    HasUK
      (UK.relevance convertedWiderExampleData exampleUK)
      (PlanData.whole convertedWiderExampleData) := by
  apply UK.hasUK_of_legIndexHasUK convertedWiderExampleData exampleUK
    (⟨4, by decide⟩ : Fin 10)
  unfold UK.legIndexHasUK
  right
  left
  exact
    ⟨20, by native_decide, by
      unfold exampleUK
      exact Or.inl rfl⟩

#eval extractIndexes convertedWiderExampleData exampleUK convertedWiderExampleHasUK

end Examples

end Model.IntervalCombined

end Formal.Categorical
