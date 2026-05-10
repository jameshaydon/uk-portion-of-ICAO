import Mathlib.CategoryTheory.Category.Basic
import Mathlib.Order.Lattice
import Mathlib.Tactic

universe u v

namespace CategoryTheory.Category

/--
Endpoint-indexed plan morphisms for an explicit mathlib category structure.

Mathlib categories are typeclass-based, but this directory keeps several
category structures over the same endpoint type at once. This abbrev lets those
structures be passed explicitly while still using mathlib's `Category`.
-/
abbrev Plan {Point : Type u} (C : Category.{v} Point) (a b : Point) : Type v :=
  @Quiver.Hom Point C.toCategoryStruct.toQuiver a b

end CategoryTheory.Category

namespace Formal.Categorical

open CategoryTheory (Category)

namespace Plan

variable {Point : Type u}
variable {C : Category Point}

abbrev comp
    {a b c : Point}
    (p : C.Plan a b)
    (q : C.Plan b c) : C.Plan a c :=
  C.comp p q

end Plan

infixl:70 " ≫ " => Plan.comp

/--
A morphism that is the categorical identity, up to the endpoint equality needed
to type the identity morphism.
-/
def IsIdPlan
    {Point : Type u}
    {C : Category Point}
    {a b : Point}
    (p : C.Plan a b) : Prop :=
  ∃ h : b = a, h ▸ p = C.id a

/--
A located occurrence of a middle plan inside a fixed whole plan.

The prefix and suffix are data. Equal middle plans at different positions are
therefore different occurrences.
-/
structure Subplan
    {Point : Type _}
    (C : Category Point)
    {a b : Point}
    (whole : C.Plan a b) where
  i : Point
  j : Point
  pre : C.Plan a i
  middle : C.Plan i j
  post : C.Plan j b
  factor : pre ≫ middle ≫ post = whole

namespace Subplan

variable {Point : Type u}
variable {C : Category Point}

/-- The trivial occurrence `[id | whole | id]`. -/
def whole
    (C : Category Point)
    {a b : Point}
    (p : C.Plan a b) : Subplan C p where
  i := a
  j := b
  pre := C.id a
  middle := p
  post := C.id b
  factor := by grind

/--
The data witnessing located containment of subplans.

`small` is contained in `large` when `large.middle` factors as left context,
`small.middle`, and right context, with matching outer contexts in the whole.
-/
structure ContainsData
    {a b : Point}
    {whole : C.Plan a b}
    (small large : Subplan C whole) where
  left : C.Plan large.i small.i
  right : C.Plan small.j large.j
  middle_factor : left ≫ small.middle ≫ right = large.middle
  pre_factor : large.pre ≫ left = small.pre
  post_factor : right ≫ large.post = small.post

/-- Mathlib's `≤` on subplans is located subplan containment. -/
instance instLE
    {a b : Point}
    {whole : C.Plan a b} : LE (Subplan C whole) where
  le small large := Nonempty (ContainsData small large)

/-- Every occurrence is contained in the trivial whole occurrence. -/
theorem le_whole
    {a b : Point}
    {whole : C.Plan a b}
    (s : Subplan C whole) : s ≤ Subplan.whole C whole :=
  ⟨{
    left := s.pre
    right := s.post
    middle_factor := s.factor
    pre_factor := by grind [Subplan.whole]
    post_factor := by grind [Subplan.whole]
  }⟩

theorem le_trans
    {a b : Point}
    {whole : C.Plan a b}
    {x y z : Subplan C whole}
    (hxy : x ≤ y)
    (hyz : y ≤ z) : x ≤ z := by
  rcases hxy with ⟨xy⟩
  rcases hyz with ⟨yz⟩
  refine ⟨{
    left := yz.left ≫ xy.left
    right := xy.right ≫ yz.right
    middle_factor := ?_
    pre_factor := ?_
    post_factor := ?_
  }⟩ <;> grind
    [xy.middle_factor, yz.middle_factor, xy.pre_factor, yz.pre_factor,
      xy.post_factor, yz.post_factor]

end Subplan

/-- A front boundary decomposition exposing the first leg. -/
structure FrontDecomposition
    {Point : Type u}
    (C : Category Point)
    (Leg : {a b : Point} -> C.Plan a b -> Prop)
    {a b : Point}
    (p : C.Plan a b) where
  cut : Point
  first : C.Plan a cut
  rest : C.Plan cut b
  first_isLeg : Leg first
  factor : first ≫ rest = p

/-- A back boundary decomposition exposing the last leg. -/
structure BackDecomposition
    {Point : Type u}
    (C : Category Point)
    (Leg : {a b : Point} -> C.Plan a b -> Prop)
    {a b : Point}
    (p : C.Plan a b) where
  cut : Point
  init : C.Plan a cut
  last : C.Plan cut b
  last_isLeg : Leg last
  factor : init ≫ last = p

/-- The remainder obtained by peeling the first leg of a located subplan. -/
def frontPeel
    {Point : Type u}
    {C : Category Point}
    {Leg : {a b : Point} -> C.Plan a b -> Prop}
    {a b : Point}
    {whole : C.Plan a b}
    (current : Subplan C whole)
    (front : FrontDecomposition C Leg current.middle) :
    Subplan C whole where
  i := front.cut
  j := current.j
  pre := current.pre ≫ front.first
  middle := front.rest
  post := current.post
  factor := by
    have hfront := front.factor
    have hcurrent := current.factor
    grind

/-- The first-leg boundary exposed by a front decomposition. -/
def frontHead
    {Point : Type u}
    {C : Category Point}
    {Leg : {a b : Point} -> C.Plan a b -> Prop}
    {a b : Point}
    {whole : C.Plan a b}
    (current : Subplan C whole)
    (front : FrontDecomposition C Leg current.middle) :
    Subplan C whole where
  i := current.i
  j := front.cut
  pre := current.pre
  middle := front.first
  post := front.rest ≫ current.post
  factor := by
    have hfront := front.factor
    have hcurrent := current.factor
    grind

/-- The initial segment obtained by peeling the last leg of a located subplan. -/
def backInitCandidate
    {Point : Type u}
    {C : Category Point}
    {Leg : {a b : Point} -> C.Plan a b -> Prop}
    {a b : Point}
    {whole : C.Plan a b}
    (current : Subplan C whole)
    (back : BackDecomposition C Leg current.middle) :
    Subplan C whole where
  i := current.i
  j := back.cut
  pre := current.pre
  middle := back.init
  post := back.last ≫ current.post
  factor := by
    have hback := back.factor
    have hcurrent := current.factor
    grind

/-- The last-leg boundary exposed by a back decomposition. -/
def backBoundaryCandidate
    {Point : Type u}
    {C : Category Point}
    {Leg : {a b : Point} -> C.Plan a b -> Prop}
    {a b : Point}
    {whole : C.Plan a b}
    (current : Subplan C whole)
    (back : BackDecomposition C Leg current.middle) :
    Subplan C whole where
  i := back.cut
  j := current.j
  pre := current.pre ≫ back.init
  middle := back.last
  post := current.post
  factor := by
    have hback := back.factor
    have hcurrent := current.factor
    grind

/--
Abstract finite free path structure over one point type.

The fields record the algebra needed by extraction and reconciliation without
choosing a list, array, or raw-index representation.

Categorically, this is the local interface this development uses for a
"category of paths" in the sense of Spielberg-style path categories: morphisms
compose like paths, have a length, split uniquely into generating legs at each
end, and have no nontrivial one-sided inverses. Mathlib contains several nearby
pieces but not this exact bundled predicate. In particular, the cancellation
fields below are the explicit-data analogue of requiring every morphism to be
both `CategoryTheory.Mono` and `CategoryTheory.Epi`; mathlib's per-morphism
typeclasses have defining fields named `right_cancellation` and
`left_cancellation`. Mathlib also has the free category on a quiver as
`CategoryTheory.Paths` in `Mathlib.CategoryTheory.PathCategory.Basic`, together
with its universal property, but that is one source of examples rather than an
abstract `IsCategoryOfPaths` class.

The length and identity laws imply there are no nontrivial one-sided inverses.
In common category-theory terminology this is close to saying the category is
gaunt, while `Quiver.IsThin` is a stronger mathlib-adjacent condition because it
allows at most one morphism between any two objects. Unique front/back
decompositions are also reminiscent of Reedy direct categories, although this
file packages only the properties needed by the flight-plan extraction argument.
-/
structure FreeFinitePlanSystem (Point : Type u) where
  cat : Category.{v} Point
  IsLeg : {a b : Point} -> cat.Plan a b -> Prop
  length : {a b : Point} -> cat.Plan a b -> Nat
  id_iff_length_zero :
    {a b : Point} -> (p : cat.Plan a b) -> IsIdPlan p <-> length p = 0
  comp_length :
    {a b c : Point} -> (p : cat.Plan a b) -> (q : cat.Plan b c) ->
      length (p ≫ q) = length p + length q
  /--
  Left multiplication by any plan is injective.

  For an implicit mathlib category, this is the cancellation supplied by
  `CategoryTheory.Epi p`.
  -/
  left_cancel :
    {a b c : Point} -> (p : cat.Plan a b) -> {q r : cat.Plan b c} ->
      p ≫ q = p ≫ r -> q = r
  /--
  Right multiplication by any plan is injective.

  For an implicit mathlib category, this is the cancellation supplied by
  `CategoryTheory.Mono r`.
  -/
  right_cancel :
    {a b c : Point} -> {p q : cat.Plan a b} -> (r : cat.Plan b c) ->
      p ≫ r = q ≫ r -> p = q
  leg_length_positive :
    {a b : Point} -> {p : cat.Plan a b} -> IsLeg p -> 0 < length p
  frontDecomposition? :
    {a b : Point} -> (p : cat.Plan a b) ->
      Option (FrontDecomposition cat IsLeg p)
  backDecomposition? :
    {a b : Point} -> (p : cat.Plan a b) ->
      Option (BackDecomposition cat IsLeg p)
  frontDecomposition?_eq_none_iff_identity :
    {a b : Point} -> (p : cat.Plan a b) ->
      frontDecomposition? p = none <-> IsIdPlan p
  backDecomposition?_eq_none_iff_identity :
    {a b : Point} -> (p : cat.Plan a b) ->
      backDecomposition? p = none <-> IsIdPlan p
  /--
  Atomic containment in a front-decomposed subplan splits across the exposed
  first-leg boundary and the remaining suffix.
  -/
  front_atom_le_boundary_or_remainder :
    {a b : Point} -> {whole : cat.Plan a b} ->
      (current : Subplan cat whole) ->
      (front : FrontDecomposition cat IsLeg current.middle) ->
      (atom : Subplan cat whole) ->
      (IsIdPlan atom.middle ∨ IsLeg atom.middle) ->
        atom <= current ->
          atom <= frontHead current front ∨
            atom <= frontPeel current front
  /--
  Atomic containment in a back-decomposed subplan splits across the remaining
  prefix and the exposed last-leg boundary.
  -/
  back_atom_le_init_or_boundary :
    {a b : Point} -> {whole : cat.Plan a b} ->
      (current : Subplan cat whole) ->
      (back : BackDecomposition cat IsLeg current.middle) ->
      (atom : Subplan cat whole) ->
      (IsIdPlan atom.middle ∨ IsLeg atom.middle) ->
        atom <= current ->
          atom <= backInitCandidate current back ∨
            atom <= backBoundaryCandidate current back
abbrev PlanSystem := FreeFinitePlanSystem

namespace FreeFinitePlanSystem

variable {Point : Type u}

/-
Derived lemmas and order instances for this interface live in
`Formal.Categorical.Core.Proofs`.
-/

abbrev Plan
    (S : PlanSystem Point)
    (a b : Point) : Type v :=
  S.cat.Plan a b

abbrev id
    (S : PlanSystem Point)
    (a : Point) : S.Plan a a :=
  S.cat.id a

abbrev comp
    (S : PlanSystem Point)
    {a b c : Point}
    (p : S.Plan a b)
    (q : S.Plan b c) : S.Plan a c :=
  p ≫ q

end FreeFinitePlanSystem

end Formal.Categorical
