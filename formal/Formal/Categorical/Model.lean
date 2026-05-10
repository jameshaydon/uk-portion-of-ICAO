import Formal.Categorical.Core

namespace Formal.Categorical.Model

universe u v

/--
An endpoint-indexed version of the concrete plan shape from `app/Main.hs`.

`Plan.nil p` is the Haskell `End p`.  `Plan.cons leg rest` is the Haskell
`Leg p leg rest`, with the start point carried by the source index and the
next waypoint carried by the source index of `rest`.
-/
inductive Plan (Point : Type u) (Route : Point -> Point -> Type v) :
    Point -> Point -> Type (max u v) where
  | nil (point : Point) :
      Plan Point Route point point
  | cons
      {start mid finish : Point}
      (here : Route start mid)
      (rest : Plan Point Route mid finish) :
      Plan Point Route start finish

namespace Plan

variable {Point : Type u}
variable {Route : Point -> Point -> Type v}

/-- The number of route legs in a concrete plan. -/
def length : Plan Point Route a b -> Nat
  | nil _ => 0
  | cons _ rest => rest.length + 1

/-- Categorical composition is concatenation of concrete plans. -/
def append :
    Plan Point Route a b -> Plan Point Route b c -> Plan Point Route a c
  | nil _, right => right
  | cons here rest, right => cons here (append rest right)

@[simp] theorem append_nil (plan : Plan Point Route a b) :
    append plan (nil b) = plan := by
  induction plan with
  | nil _ => rfl
  | cons here rest ih =>
      simp [append, ih]

@[simp] theorem append_assoc
    (left : Plan Point Route a b)
    (middle : Plan Point Route b c)
    (right : Plan Point Route c d) :
    append (append left middle) right =
      append left (append middle right) := by
  induction left with
  | nil _ => rfl
  | cons here rest ih =>
      simp [append, ih]

theorem append_comp_tail
    {a b c d : Point}
    {left : Plan Point Route a b}
    {middle : Plan Point Route b c}
    {right : Plan Point Route c d}
    {pref : Plan Point Route a c}
    {whole : Plan Point Route a d}
    (hpref : append left middle = pref)
    (hwhole : append pref right = whole) :
    append left (append middle right) = whole := by
  calc
    append left (append middle right) = append (append left middle) right :=
      (append_assoc left middle right).symm
    _ = append pref right := by
      rw [hpref]
    _ = whole := hwhole

instance category
    (Point : Type u)
    (Route : Point -> Point -> Type v) : CategoryTheory.Category Point where
  Hom a b := Plan Point Route a b
  id a := nil a
  comp left right := append left right
  id_comp := by
    intro _ _ _
    rfl
  comp_id := by
    intro _ _ plan
    exact append_nil plan
  assoc := by
    intro _ _ _ _ left middle right
    exact append_assoc left middle right

@[simp] theorem length_append
    (left : Plan Point Route a b)
    (right : Plan Point Route b c) :
    length (append left right) = length left + length right := by
  induction left with
  | nil _ =>
      simp [append, length]
  | cons here rest ih =>
      simp [append, length, ih, Nat.add_assoc]
      omega

@[simp] theorem length_nil (point : Point) :
    length (nil (Route := Route) point) = 0 := rfl

/-- A one-route concrete plan. -/
def single (route : Route a b) : Plan Point Route a b :=
  cons route (nil b)

/-- The concrete generating legs are exactly one-route plans. -/
def IsLeg {a b : Point} (plan : Plan Point Route a b) : Prop :=
  ∃ route : Route a b, plan = single route

theorem leg_length_positive
    {a b : Point}
    {plan : Plan Point Route a b}
    (h : IsLeg plan) : 0 < length plan := by
  rcases h with ⟨route, rfl⟩
  simp [single, length]

theorem leg_length_eq_one
    {a b : Point}
    {plan : Plan Point Route a b}
    (h : IsLeg plan) : length plan = 1 := by
  rcases h with ⟨route, rfl⟩
  simp [single, length]

theorem isIdPlan_iff_length_zero
    {a b : Point}
    (plan : Plan Point Route a b) :
    IsIdPlan (C := category Point Route) plan <-> length plan = 0 := by
  constructor
  · intro h
    rcases h with ⟨hend, hplan⟩
    cases hend
    simpa using congrArg length hplan
  · intro hlen
    cases plan with
    | nil point =>
        exact ⟨rfl, rfl⟩
    | cons here rest =>
        simp [length] at hlen

def frontDecomposition? :
    {a b : Point} ->
    (plan : Plan Point Route a b) ->
    Option
      (FrontDecomposition
        (category Point Route)
        (@IsLeg Point Route)
        plan)
  | _, _, nil _ => none
  | _, _, cons here rest =>
      some
        { cut := _
          first := single here
          rest := rest
          first_isLeg := ⟨here, rfl⟩
          factor := rfl }

theorem append_left_cancel
    {a b c : Point}
    (left : Plan Point Route a b)
    {right₁ right₂ : Plan Point Route b c}
    (h : append left right₁ = append left right₂) :
    right₁ = right₂ := by
  induction left with
  | nil point =>
      simpa [append] using h
  | cons here rest ih =>
      simp [append] at h
      exact ih h

theorem append_right_cancel
    {a b c : Point}
    {left₁ left₂ : Plan Point Route a b}
    (right : Plan Point Route b c)
    (h : append left₁ right = append left₂ right) :
    left₁ = left₂ := by
  induction left₁ with
  | nil point =>
      cases left₂ with
      | nil _ =>
          rfl
      | cons here rest =>
          have hlen := congrArg length h
          simp [length_append, length] at hlen
  | cons here rest ih =>
      cases left₂ with
      | nil point =>
          have hlen := congrArg length h
          simp [length_append, length] at hlen
      | cons there rest₂ =>
          simp [append] at h
          rcases h with ⟨hcut, hroute, hrest⟩
          cases hcut
          cases hroute
          exact congrArg (cons here) (ih right (eq_of_heq hrest))

/-- The left side of a binary concatenation, located inside the whole. -/
@[simp]
def splitLeftCandidate
    {Point : Type u}
    {C : CategoryTheory.Category.{v} Point}
    {a b c : Point}
    (left : C.Plan a b)
    (right : C.Plan b c) :
    Subplan C (left ≫ right) where
  i := a
  j := b
  pre := C.id a
  middle := left
  post := right
  factor := by grind

/-- The right side of a binary concatenation, located inside the whole. -/
@[simp]
def splitRightCandidate
    {Point : Type u}
    {C : CategoryTheory.Category.{v} Point}
    {a b c : Point}
    (left : C.Plan a b)
    (right : C.Plan b c) :
    Subplan C (left ≫ right) where
  i := b
  j := c
  pre := left
  middle := right
  post := C.id c
  factor := by grind

/-- The shared cut point of a binary concatenation, located inside the whole. -/
@[simp]
def splitBoundaryCandidate
    {Point : Type u}
    {C : CategoryTheory.Category.{v} Point}
    {a b c : Point}
    (left : C.Plan a b)
    (right : C.Plan b c) :
    Subplan C (left ≫ right) where
  i := b
  j := b
  pre := left
  middle := C.id b
  post := right
  factor := by grind

attribute [local simp]
  frontPeel
  frontHead
  backInitCandidate
  backBoundaryCandidate

/-- A leg occurrence lies on exactly one side of a binary split. -/
def SplitLegLocation
    {Point : Type u}
    {C : CategoryTheory.Category.{v} Point}
    {a b c : Point}
    (left : C.Plan a b)
    (right : C.Plan b c)
    (atom : Subplan C (left ≫ right)) : Prop :=
  (atom <= splitLeftCandidate left right ∧
      ¬ atom <= splitRightCandidate left right) ∨
    (atom <= splitRightCandidate left right ∧
      ¬ atom <= splitLeftCandidate left right)

/--
A point occurrence either lies on exactly one side of a binary split, or is
the shared cut point.
-/
def SplitPointLocation
    {Point : Type u}
    {C : CategoryTheory.Category.{v} Point}
    {a b c : Point}
    (left : C.Plan a b)
    (right : C.Plan b c)
    (atom : Subplan C (left ≫ right)) : Prop :=
  SplitLegLocation left right atom ∨
    atom = splitBoundaryCandidate left right

def backDecomposition? :
    {a b : Point} ->
    (plan : Plan Point Route a b) ->
    Option
      (BackDecomposition
        (category Point Route)
        (@IsLeg Point Route)
        plan)
  | _, _, nil _ => none
  | start, _, cons here (nil _) =>
      some
        { cut := start
          init := nil start
          last := single here
          last_isLeg := ⟨here, rfl⟩
          factor := rfl }
  | _, _, cons here (cons next rest) =>
      match backDecomposition? (cons next rest) with
      | none => none
      | some back =>
          some
            { cut := back.cut
              init := cons here back.init
              last := back.last
              last_isLeg := back.last_isLeg
              factor := by
                have hfactor : append back.init back.last =
                    cons next rest := by
                  simpa [category] using back.factor
                change append (cons here back.init) back.last =
                  cons here (cons next rest)
                simp [append, hfactor] }

theorem frontDecomposition?_none_iff_identity
    {a b : Point}
    (plan : Plan Point Route a b) :
    frontDecomposition? plan = none <->
      IsIdPlan (C := category Point Route) plan := by
  rw [isIdPlan_iff_length_zero]
  cases plan <;> simp [frontDecomposition?, length]

theorem backDecomposition?_none_iff_identity :
    {a b : Point} ->
    (plan : Plan Point Route a b) ->
    backDecomposition? plan = none <->
      IsIdPlan (C := category Point Route) plan
  | _, _, nil _ => by
      rw [isIdPlan_iff_length_zero]
      simp [backDecomposition?, length]
  | _, _, cons here (nil _) => by
      rw [isIdPlan_iff_length_zero]
      simp [backDecomposition?, length]
  | _, _, cons here (cons next rest) => by
      constructor
      · intro hdecomp
        cases htail :
            backDecomposition? (cons next rest) with
        | none =>
            have htail_id :
                IsIdPlan (C := category Point Route) (cons next rest) :=
              (backDecomposition?_none_iff_identity (cons next rest)).1 htail
            have hlen : length (cons next rest) = 0 :=
              (isIdPlan_iff_length_zero (cons next rest)).1 htail_id
            simp [length] at hlen
        | some back =>
            simp [backDecomposition?, htail] at hdecomp
      · intro hid
        have hlen : length (cons here (cons next rest)) = 0 :=
          (isIdPlan_iff_length_zero (cons here (cons next rest))).1 hid
        simp [length] at hlen

inductive PrefixComparison
    {a i b c : Point}
    (pre : Plan Point Route a i)
    (tail : Plan Point Route i c)
    (left : Plan Point Route a b)
    (right : Plan Point Route b c) : Prop where
  | prePrefix
      (bridge : Plan Point Route i b)
      (left_factor : append pre bridge = left)
      (tail_factor : append bridge right = tail) :
      PrefixComparison pre tail left right
  | leftPrefix
      (bridge : Plan Point Route b i)
      (pre_factor : append left bridge = pre)
      (right_factor : append bridge tail = right) :
      PrefixComparison pre tail left right

theorem prefixComparison
    {a i b c : Point}
    (pre : Plan Point Route a i)
    (tail : Plan Point Route i c)
    (left : Plan Point Route a b)
    (right : Plan Point Route b c)
    (h : append pre tail = append left right) :
    PrefixComparison pre tail left right := by
  induction pre with
  | nil point =>
      exact PrefixComparison.prePrefix left (by rfl) (by simpa [append] using h.symm)
  | cons here rest ih =>
      cases left with
      | nil point =>
          exact PrefixComparison.leftPrefix (cons here rest) (by rfl) (by simpa [append] using h)
      | cons there leftRest =>
          simp [append] at h
          rcases h with ⟨hcut, hroute, htail⟩
          cases hcut
          cases hroute
          cases ih tail leftRest (eq_of_heq htail) with
          | prePrefix bridge hleft htail' =>
              exact PrefixComparison.prePrefix bridge
                (by simp [append, hleft])
                htail'
          | leftPrefix bridge hpre hright =>
              exact PrefixComparison.leftPrefix bridge
                (by simp [append, hpre])
                hright

theorem not_right_of_left_leg
    {a b c : Point}
    (left : Plan Point Route a b)
    (right : Plan Point Route b c)
    (atom : Subplan (category Point Route) (left ≫ right))
    (hleg : IsLeg atom.middle)
    (hleft : atom <= splitLeftCandidate (C := category Point Route) left right) :
    ¬ atom <= splitRightCandidate (C := category Point Route) left right := by
  intro hright
  rcases hleft with ⟨leftData⟩
  rcases hright with ⟨rightData⟩
  have hpos : 0 < length atom.middle := leg_length_positive hleg
  have hpre_left := congrArg length leftData.pre_factor
  change length (append (nil _) leftData.left) = length atom.pre at hpre_left
  simp [append] at hpre_left
  have hmiddle_left := congrArg length leftData.middle_factor
  change length (append (append leftData.left atom.middle) leftData.right) =
    length left at hmiddle_left
  simp [length_append] at hmiddle_left
  have hpre_right := congrArg length rightData.pre_factor
  change length (append left rightData.left) = length atom.pre at hpre_right
  simp [length_append] at hpre_right
  omega

theorem not_left_of_right_leg
    {a b c : Point}
    (left : Plan Point Route a b)
    (right : Plan Point Route b c)
    (atom : Subplan (category Point Route) (left ≫ right))
    (hleg : IsLeg atom.middle)
    (hright : atom <= splitRightCandidate (C := category Point Route) left right) :
    ¬ atom <= splitLeftCandidate (C := category Point Route) left right := by
  intro hleft
  exact not_right_of_left_leg left right atom hleg hleft hright

theorem atom_le_split_left
    {a b c : Point}
    {left : Plan Point Route a b}
    {right : Plan Point Route b c}
    (atom : Subplan (category Point Route) (left ≫ right))
    {ctx : Plan Point Route atom.j b}
    (hleft : append (append atom.pre atom.middle) ctx = left)
    (hpost : append ctx right = atom.post) :
    atom <= splitLeftCandidate (C := category Point Route) left right := by
  refine ⟨{
    left := atom.pre
    right := ctx
    middle_factor := ?_
    pre_factor := ?_
    post_factor := ?_
  }⟩
  · exact hleft
  · simp
  · exact hpost

theorem atom_le_split_right
    {a b c : Point}
    {left : Plan Point Route a b}
    {right : Plan Point Route b c}
    (atom : Subplan (category Point Route) (left ≫ right))
    {ctx : Plan Point Route b atom.i}
    (hpre : append left ctx = atom.pre)
    (hright : append (append ctx atom.middle) atom.post = right) :
    atom <= splitRightCandidate (C := category Point Route) left right := by
  refine ⟨{
    left := ctx
    right := atom.post
    middle_factor := ?_
    pre_factor := ?_
    post_factor := ?_
  }⟩
  · exact hright
  · exact hpre
  · simp

theorem not_right_of_pre_short
    {a b c : Point}
    (left : Plan Point Route a b)
    (right : Plan Point Route b c)
    (atom : Subplan (category Point Route) (left ≫ right))
    (hshort : length atom.pre < length left) :
    ¬ atom <= splitRightCandidate (C := category Point Route) left right := by
  intro hright
  rcases hright with ⟨rightData⟩
  have hpre_right := congrArg length rightData.pre_factor
  change length (append left rightData.left) = length atom.pre at hpre_right
  simp [length_append] at hpre_right
  omega

theorem not_left_of_pre_long
    {a b c : Point}
    (left : Plan Point Route a b)
    (right : Plan Point Route b c)
    (atom : Subplan (category Point Route) (left ≫ right))
    (hlong : length left < length atom.pre) :
    ¬ atom <= splitLeftCandidate (C := category Point Route) left right := by
  intro hleft
  rcases hleft with ⟨leftData⟩
  have hpre_left := congrArg length leftData.pre_factor
  change length (append (nil _) leftData.left) = length atom.pre at hpre_left
  simp [append] at hpre_left
  have hmiddle_left := congrArg length leftData.middle_factor
  change length (append (append leftData.left atom.middle) leftData.right) =
    length left at hmiddle_left
  simp [length_append] at hmiddle_left
  omega

theorem leg_le_split_left_of_pre_lt
    {a b c : Point}
    (left : Plan Point Route a b)
    (right : Plan Point Route b c)
    (atom : Subplan (category Point Route) (left ≫ right))
    (hleg : IsLeg atom.middle)
    (hshort : length atom.pre < length left) :
    atom <= splitLeftCandidate (C := category Point Route) left right := by
  cases atom with
  | mk i j pre middle post factor =>
  have hfactor : append (append pre middle) post =
      append left right := by
    simpa [category, append_assoc] using factor
  cases prefixComparison (append pre middle) post left right
      hfactor with
  | prePrefix ctx hleft hpost =>
      exact atom_le_split_left
        ({ i := i, j := j, pre := pre, middle := middle, post := post,
           factor := factor } :
          Subplan (category Point Route) (left ≫ right))
        hleft hpost
  | leftPrefix ctx hpre hright =>
      have hmid : length middle = 1 := leg_length_eq_one hleg
      have hshort' : length pre < length left := hshort
      have hlen := congrArg length hpre
      simp [length_append, hmid] at hlen
      have hctx_zero : length ctx = 0 := by omega
      rcases (isIdPlan_iff_length_zero ctx).2 hctx_zero with
        ⟨hend, hctx⟩
      cases hend
      change ctx = nil b at hctx
      subst ctx
      exact atom_le_split_left
        ({ i := i, j := b, pre := pre, middle := middle, post := post,
           factor := factor } :
          Subplan (category Point Route) (left ≫ right))
        (ctx := nil b)
        (by simpa [append_nil] using hpre.symm)
        (by simpa [append] using hright.symm)

theorem leg_le_split_right_of_pre_ge
    {a b c : Point}
    (left : Plan Point Route a b)
    (right : Plan Point Route b c)
    (atom : Subplan (category Point Route) (left ≫ right))
    (hge : length left <= length atom.pre) :
    atom <= splitRightCandidate (C := category Point Route) left right := by
  cases atom with
  | mk i j pre middle post factor =>
  have hfactor : append pre (append middle post) = append left right := by
    simpa [category, append_assoc] using factor
  cases prefixComparison pre (append middle post) left right
      hfactor with
  | prePrefix ctx hleft htail =>
      have hge' : length left <= length pre := hge
      have hlen := congrArg length hleft
      simp [length_append] at hlen
      have hctx_zero : length ctx = 0 := by omega
      rcases (isIdPlan_iff_length_zero ctx).2 hctx_zero with
        ⟨hend, hctx⟩
      cases hend
      change ctx = nil b at hctx
      subst ctx
      exact atom_le_split_right
        ({ i := b, j := j, pre := pre, middle := middle, post := post,
           factor := factor } :
          Subplan (category Point Route) (left ≫ right))
        (ctx := nil b)
        (by simpa [append_nil] using hleft.symm)
        (by simpa [append] using htail.symm)
  | leftPrefix ctx hpre hright =>
      exact atom_le_split_right
        ({ i := i, j := j, pre := pre, middle := middle, post := post,
           factor := factor } :
          Subplan (category Point Route) (left ≫ right))
        hpre
        (by simpa [append_assoc] using hright)

theorem atom_leg_location_in_split
    {a b c : Point}
    (left : Plan Point Route a b)
    (right : Plan Point Route b c)
    (atom : Subplan (category Point Route) (left ≫ right))
    (hleg : IsLeg atom.middle) :
    SplitLegLocation (C := category Point Route) left right atom := by
  by_cases hshort : length atom.pre < length left
  · left
    have hleft := leg_le_split_left_of_pre_lt left right atom hleg hshort
    exact ⟨hleft, not_right_of_left_leg left right atom hleg hleft⟩
  · right
    have hright := leg_le_split_right_of_pre_ge left right atom (Nat.le_of_not_gt hshort)
    exact ⟨hright, not_left_of_right_leg left right atom hleg hright⟩

theorem point_le_split_left_of_pre_le
    {a b c : Point}
    (left : Plan Point Route a b)
    (right : Plan Point Route b c)
    (atom : Subplan (category Point Route) (left ≫ right))
    (hid : IsIdPlan atom.middle)
    (hle : length atom.pre <= length left) :
    atom <= splitLeftCandidate (C := category Point Route) left right := by
  cases atom with
  | mk i j pre middle post factor =>
  have hfactor : append (append pre middle) post = append left right := by
    simpa [category, append_assoc] using factor
  cases prefixComparison (append pre middle) post left right hfactor with
  | prePrefix ctx hleft hpost =>
      exact atom_le_split_left
        ({ i := i, j := j, pre := pre, middle := middle, post := post,
           factor := factor } :
          Subplan (category Point Route) (left ≫ right))
        hleft hpost
  | leftPrefix ctx hpre hright =>
      have hmid : length middle = 0 :=
        (isIdPlan_iff_length_zero middle).1 hid
      have hle' : length pre <= length left := hle
      have hlen := congrArg length hpre
      simp [length_append, hmid] at hlen
      have hctx_zero : length ctx = 0 := by omega
      rcases (isIdPlan_iff_length_zero ctx).2 hctx_zero with
        ⟨hend, hctx⟩
      cases hend
      change ctx = nil b at hctx
      subst ctx
      exact atom_le_split_left
        ({ i := i, j := b, pre := pre, middle := middle, post := post,
           factor := factor } :
          Subplan (category Point Route) (left ≫ right))
        (ctx := nil b)
        (by simpa [append_nil] using hpre.symm)
        (by simpa [append] using hright.symm)

theorem point_le_split_right_of_pre_ge
    {a b c : Point}
    (left : Plan Point Route a b)
    (right : Plan Point Route b c)
    (atom : Subplan (category Point Route) (left ≫ right))
    (hge : length left <= length atom.pre) :
    atom <= splitRightCandidate (C := category Point Route) left right :=
  leg_le_split_right_of_pre_ge left right atom hge

theorem point_eq_split_boundary_of_pre_length
    {a b c : Point}
    (left : Plan Point Route a b)
    (right : Plan Point Route b c)
    (atom : Subplan (category Point Route) (left ≫ right))
    (hid : IsIdPlan atom.middle)
    (hlen_pre : length atom.pre = length left) :
    atom = splitBoundaryCandidate (C := category Point Route) left right := by
  cases atom with
  | mk i j pre middle post factor =>
      rcases hid with ⟨hend, hmiddle⟩
      cases hend
      change middle = nil i at hmiddle
      subst middle
      have hlen_pre' : length pre = length left := hlen_pre
      have hfactor : append pre post = append left right := by
        simpa [category, append, append_assoc] using factor
      cases prefixComparison pre post left right hfactor with
      | prePrefix ctx hleft hpost =>
          have hctx_zero : length ctx = 0 := by
            have hlen := congrArg length hleft
            simp [length_append] at hlen
            omega
          rcases (isIdPlan_iff_length_zero ctx).2 hctx_zero with
            ⟨hctx_endpoint, hctx⟩
          cases hctx_endpoint
          change ctx = nil b at hctx
          subst ctx
          have hpre_eq : pre = left := by
            rw [← hleft]
            simp [append_nil]
          have hpost_eq : post = right := by
            simpa [append] using hpost.symm
          cases hpre_eq
          cases hpost_eq
          rfl
      | leftPrefix ctx hpre hright =>
          have hctx_zero : length ctx = 0 := by
            have hlen := congrArg length hpre
            simp [length_append] at hlen
            omega
          rcases (isIdPlan_iff_length_zero ctx).2 hctx_zero with
            ⟨hctx_endpoint, hctx⟩
          cases hctx_endpoint
          change ctx = nil b at hctx
          subst ctx
          have hpre_eq : pre = left := by
            rw [← hpre]
            simp [append_nil]
          have hpost_eq : post = right := by
            rw [← hright]
            simp [append]
          cases hpre_eq
          cases hpost_eq
          rfl

theorem atom_point_location_in_split
    {a b c : Point}
    (left : Plan Point Route a b)
    (right : Plan Point Route b c)
    (atom : Subplan (category Point Route) (left ≫ right))
    (hid : IsIdPlan atom.middle) :
    SplitPointLocation (C := category Point Route) left right atom := by
  rcases lt_trichotomy (length atom.pre) (length left) with hlt | heq | hgt
  · left
    have hleft := point_le_split_left_of_pre_le left right atom hid (Nat.le_of_lt hlt)
    exact Or.inl ⟨hleft, not_right_of_pre_short left right atom hlt⟩
  · right
    exact point_eq_split_boundary_of_pre_length left right atom hid heq
  · left
    have hright := point_le_split_right_of_pre_ge left right atom (Nat.le_of_lt hgt)
    exact Or.inr ⟨hright, not_left_of_pre_long left right atom hgt⟩

theorem front_local_left_le_boundary
    {a b : Point}
    {whole : Plan Point Route a b}
    (current : Subplan (category Point Route) whole)
    (front : FrontDecomposition
      (category Point Route)
      (@IsLeg Point Route)
      current.middle)
    (atom : Subplan (category Point Route) whole)
    (curData : Subplan.ContainsData atom current)
    (hlocal :
      ({ i := atom.i
         j := atom.j
         pre := curData.left
         middle := atom.middle
         post := curData.right
         factor := by
           have h := curData.middle_factor
           simpa [front.factor] using h } :
        Subplan (category Point Route) (front.first ≫ front.rest)) <=
      splitLeftCandidate (C := category Point Route) front.first front.rest) :
    atom <= frontHead current front := by
  rcases hlocal with ⟨localData⟩
  refine ⟨{
    left := localData.left
    right := localData.right
    middle_factor := localData.middle_factor
    pre_factor := ?_
    post_factor := ?_
  }⟩
  · have hpre := localData.pre_factor
    have hcur := curData.pre_factor
    have hpre' : localData.left = curData.left := by
      simpa using hpre
    simpa [frontHead, hpre'] using hcur
  · have hpost := localData.post_factor
    have hcur := curData.post_factor
    have hpost' : localData.right ≫ front.rest = curData.right := by
      simpa using hpost
    simpa [frontHead] using append_comp_tail hpost' hcur

theorem front_local_right_le_remainder
    {a b : Point}
    {whole : Plan Point Route a b}
    (current : Subplan (category Point Route) whole)
    (front : FrontDecomposition
      (category Point Route)
      (@IsLeg Point Route)
      current.middle)
    (atom : Subplan (category Point Route) whole)
    (curData : Subplan.ContainsData atom current)
    (hlocal :
      ({ i := atom.i
         j := atom.j
         pre := curData.left
         middle := atom.middle
         post := curData.right
         factor := by
           have h := curData.middle_factor
           simpa [front.factor] using h } :
        Subplan (category Point Route) (front.first ≫ front.rest)) <=
      splitRightCandidate (C := category Point Route) front.first front.rest) :
    atom <= frontPeel current front := by
  rcases hlocal with ⟨localData⟩
  refine ⟨{
    left := localData.left
    right := localData.right
    middle_factor := localData.middle_factor
    pre_factor := ?_
    post_factor := ?_
  }⟩
  · have hpre := localData.pre_factor
    have hcur := curData.pre_factor
    have hpre' : front.first ≫ localData.left = curData.left := by
      simpa using hpre
    simpa [frontPeel, append_assoc, hpre'] using hcur
  · have hpost := localData.post_factor
    have hcur := curData.post_factor
    have hpost' : localData.right = curData.right := by
      simpa using hpost
    simpa [frontPeel, hpost'] using hcur

theorem front_atom_le_boundary_or_remainder
    {a b : Point}
    {whole : Plan Point Route a b}
    (current : Subplan (category Point Route) whole)
    (front : FrontDecomposition
      (category Point Route)
      (@IsLeg Point Route)
      current.middle)
    (atom : Subplan (category Point Route) whole)
    (hatom : IsIdPlan atom.middle ∨ IsLeg atom.middle)
    (hatom_current : atom <= current) :
      atom <= frontHead current front ∨
        atom <= frontPeel current front := by
  rcases hatom_current with ⟨curData⟩
  let localAtom : Subplan (category Point Route) (front.first ≫ front.rest) :=
    { i := atom.i
      j := atom.j
      pre := curData.left
      middle := atom.middle
      post := curData.right
      factor := by
        have h := curData.middle_factor
        simpa [front.factor] using h }
  rcases hatom with hid | hleg
  · have hloc := atom_point_location_in_split front.first front.rest localAtom hid
    rcases hloc with hlegloc | hboundary
    · rcases hlegloc with ⟨hleft, _⟩ | ⟨hright, _⟩
      · exact Or.inl (front_local_left_le_boundary current front atom curData hleft)
      · exact Or.inr (front_local_right_le_remainder current front atom curData hright)
    · left
      have hbdleft : localAtom <=
          splitLeftCandidate (C := category Point Route) front.first front.rest := by
        rw [hboundary]
        refine ⟨{
          left := front.first
          right := (category Point Route).id front.cut
          middle_factor := ?_
          pre_factor := ?_
          post_factor := ?_
        }⟩ <;> simp [splitBoundaryCandidate, splitLeftCandidate]
      exact front_local_left_le_boundary current front atom curData hbdleft
  · have hloc := atom_leg_location_in_split front.first front.rest localAtom hleg
    rcases hloc with ⟨hleft, _⟩ | ⟨hright, _⟩
    · exact Or.inl (front_local_left_le_boundary current front atom curData hleft)
    · exact Or.inr (front_local_right_le_remainder current front atom curData hright)

theorem back_local_left_le_init
    {a b : Point}
    {whole : Plan Point Route a b}
    (current : Subplan (category Point Route) whole)
    (back : BackDecomposition
      (category Point Route)
      (@IsLeg Point Route)
      current.middle)
    (atom : Subplan (category Point Route) whole)
    (curData : Subplan.ContainsData atom current)
    (hlocal :
      ({ i := atom.i
         j := atom.j
         pre := curData.left
         middle := atom.middle
         post := curData.right
         factor := by
           have h := curData.middle_factor
           simpa [back.factor] using h } :
        Subplan (category Point Route) (back.init ≫ back.last)) <=
      splitLeftCandidate (C := category Point Route) back.init back.last) :
    atom <= backInitCandidate current back := by
  rcases hlocal with ⟨localData⟩
  refine ⟨{
    left := localData.left
    right := localData.right
    middle_factor := localData.middle_factor
    pre_factor := ?_
    post_factor := ?_
  }⟩
  · have hpre := localData.pre_factor
    have hcur := curData.pre_factor
    have hpre' : localData.left = curData.left := by
      simpa using hpre
    simpa [backInitCandidate, hpre'] using hcur
  · have hpost := localData.post_factor
    have hcur := curData.post_factor
    have hpost' : localData.right ≫ back.last = curData.right := by
      simpa using hpost
    simpa [backInitCandidate] using append_comp_tail hpost' hcur

theorem back_local_right_le_boundary
    {a b : Point}
    {whole : Plan Point Route a b}
    (current : Subplan (category Point Route) whole)
    (back : BackDecomposition
      (category Point Route)
      (@IsLeg Point Route)
      current.middle)
    (atom : Subplan (category Point Route) whole)
    (curData : Subplan.ContainsData atom current)
    (hlocal :
      ({ i := atom.i
         j := atom.j
         pre := curData.left
         middle := atom.middle
         post := curData.right
         factor := by
           have h := curData.middle_factor
           simpa [back.factor] using h } :
        Subplan (category Point Route) (back.init ≫ back.last)) <=
      splitRightCandidate (C := category Point Route) back.init back.last) :
    atom <= backBoundaryCandidate current back := by
  rcases hlocal with ⟨localData⟩
  refine ⟨{
    left := localData.left
    right := localData.right
    middle_factor := localData.middle_factor
    pre_factor := ?_
    post_factor := ?_
  }⟩
  · have hpre := localData.pre_factor
    have hcur := curData.pre_factor
    have hpre' : back.init ≫ localData.left = curData.left := by
      simpa using hpre
    simpa [backBoundaryCandidate, append_assoc, hpre'] using hcur
  · have hpost := localData.post_factor
    have hcur := curData.post_factor
    have hpost' : localData.right = curData.right := by
      simpa using hpost
    simpa [backBoundaryCandidate, hpost'] using hcur

theorem back_atom_le_init_or_boundary
    {a b : Point}
    {whole : Plan Point Route a b}
    (current : Subplan (category Point Route) whole)
    (back : BackDecomposition
      (category Point Route)
      (@IsLeg Point Route)
      current.middle)
    (atom : Subplan (category Point Route) whole)
    (hatom : IsIdPlan atom.middle ∨ IsLeg atom.middle)
    (hatom_current : atom <= current) :
      atom <= backInitCandidate current back ∨
        atom <= backBoundaryCandidate current back := by
  rcases hatom_current with ⟨curData⟩
  let localAtom : Subplan (category Point Route) (back.init ≫ back.last) :=
    { i := atom.i
      j := atom.j
      pre := curData.left
      middle := atom.middle
      post := curData.right
      factor := by
        have h := curData.middle_factor
        simpa [back.factor] using h }
  rcases hatom with hid | hleg
  · have hloc := atom_point_location_in_split back.init back.last localAtom hid
    rcases hloc with hlegloc | hboundary
    · rcases hlegloc with ⟨hleft, _⟩ | ⟨hright, _⟩
      · exact Or.inl (back_local_left_le_init current back atom curData hleft)
      · exact Or.inr (back_local_right_le_boundary current back atom curData hright)
    · right
      have hbdright : localAtom <=
          splitRightCandidate (C := category Point Route) back.init back.last := by
        rw [hboundary]
        refine ⟨{
          left := (category Point Route).id back.cut
          right := back.last
          middle_factor := ?_
          pre_factor := ?_
          post_factor := ?_
        }⟩ <;> simp [splitBoundaryCandidate, splitRightCandidate]
      exact back_local_right_le_boundary current back atom curData hbdright
  · have hloc := atom_leg_location_in_split back.init back.last localAtom hleg
    rcases hloc with ⟨hleft, _⟩ | ⟨hright, _⟩
    · exact Or.inl (back_local_left_le_init current back atom curData hleft)
    · exact Or.inr (back_local_right_le_boundary current back atom curData hright)

/-!
The remaining `FreeFinitePlanSystem` fields are the path-order facts for this
concrete representation: cancellation, boundary decompositions, and the
front/back atomic containment facts used by extraction.
-/

def freeFinitePlanSystem
    (Point : Type u)
    (Route : Point -> Point -> Type v) :
    FreeFinitePlanSystem.{u, max u v} Point where
  cat := category Point Route
  IsLeg := @IsLeg Point Route
  length := @length Point Route
  id_iff_length_zero := @isIdPlan_iff_length_zero Point Route
  comp_length := @length_append Point Route
  left_cancel := @append_left_cancel Point Route
  right_cancel := @append_right_cancel Point Route
  leg_length_positive := @leg_length_positive Point Route
  frontDecomposition? := @frontDecomposition? Point Route
  backDecomposition? := @backDecomposition? Point Route
  frontDecomposition?_eq_none_iff_identity :=
    @frontDecomposition?_none_iff_identity Point Route
  backDecomposition?_eq_none_iff_identity :=
    @backDecomposition?_none_iff_identity Point Route
  front_atom_le_boundary_or_remainder :=
    @front_atom_le_boundary_or_remainder Point Route
  back_atom_le_init_or_boundary :=
    @back_atom_le_init_or_boundary Point Route

end Plan

end Formal.Categorical.Model
