import Mathlib.CategoryTheory.Category.Basic

universe u v

namespace Formal.Categorical.PathCategory

open CategoryTheory

/-- A morphism which is an identity, allowing for the endpoint equality. -/
def IsIdentity {C : Type u} [Category.{v} C] {X Y : C} (f : X ⟶ Y) : Prop :=
  ∃ h : Y = X, h ▸ f = 𝟙 X

/--
The small path-category interface used by the convexity theorem.

It is intentionally phrased only with ordinary mathlib category syntax:
objects, morphisms, `𝟙`, and `≫`.
-/
class IsPathCategory (C : Type u) [Category.{v} C] : Prop where
  left_cancel :
    {X Y Z : C} -> (f : X ⟶ Y) -> {g h : Y ⟶ Z} ->
      f ≫ g = f ≫ h -> g = h
  right_cancel :
    {X Y Z : C} -> {f g : X ⟶ Y} -> (h : Y ⟶ Z) ->
      f ≫ h = g ≫ h -> f = g
  comp_eq_id_left_identity :
    {X Y : C} -> (f : X ⟶ Y) -> (g : Y ⟶ X) ->
      f ≫ g = 𝟙 X -> IsIdentity f
  comp_eq_id_right_identity :
    {X Y : C} -> (f : X ⟶ Y) -> (g : Y ⟶ X) ->
      f ≫ g = 𝟙 X -> IsIdentity g

/--
A located subpath of a fixed whole path.

The subpath is `hom`; `pre` and `post` record its position inside `whole`.
-/
structure Sub
    {C : Type u} [Category.{v} C]
    {A B : C}
    (whole : A ⟶ B) where
  leftObj : C
  rightObj : C
  pre : A ⟶ leftObj
  hom : leftObj ⟶ rightObj
  post : rightObj ⟶ B
  factor : pre ≫ hom ≫ post = whole

namespace Sub

variable {C : Type u} [Category.{v} C]
variable {A B : C}
variable {whole : A ⟶ B}

/--
The data witnessing located containment.

`small ≤ large` means that `small` occurs inside `large`, compatibly with their
positions in the same `whole`.
-/
structure ContainsData
    (small large : Sub whole) where
  left : large.leftObj ⟶ small.leftObj
  right : small.rightObj ⟶ large.rightObj
  middle_factor : left ≫ small.hom ≫ right = large.hom
  pre_factor : large.pre ≫ left = small.pre
  post_factor : right ≫ large.post = small.post

/-- Located containment of subpaths of the same whole path. -/
def Contains (small large : Sub whole) : Prop :=
  Nonempty (ContainsData small large)

instance : LE (Sub whole) where
  le := Contains

/-- The located left subpath induced by a three-part factorization. -/
def leftPart
    (y : Sub whole)
    {K L : C}
    (a : y.leftObj ⟶ K)
    (b : K ⟶ L)
    (c : L ⟶ y.rightObj)
    (hy : a ≫ b ≫ c = y.hom) :
    Sub whole where
  leftObj := y.leftObj
  rightObj := K
  pre := y.pre
  hom := a
  post := b ≫ c ≫ y.post
  factor := by
    have hfactor := y.factor
    rw [← hy] at hfactor
    simpa [Category.assoc] using hfactor

/-- The located middle subpath induced by a three-part factorization. -/
def middle
    (y : Sub whole)
    {K L : C}
    (a : y.leftObj ⟶ K)
    (b : K ⟶ L)
    (c : L ⟶ y.rightObj)
    (hy : a ≫ b ≫ c = y.hom) :
    Sub whole where
  leftObj := K
  rightObj := L
  pre := y.pre ≫ a
  hom := b
  post := c ≫ y.post
  factor := by
    have hfactor := y.factor
    rw [← hy] at hfactor
    simpa [Category.assoc] using hfactor

/-- The located suffix subpath induced by a three-part factorization. -/
def rightPart
    (y : Sub whole)
    {K L : C}
    (a : y.leftObj ⟶ K)
    (b : K ⟶ L)
    (c : L ⟶ y.rightObj)
    (hy : a ≫ b ≫ c = y.hom) :
    Sub whole where
  leftObj := L
  rightObj := y.rightObj
  pre := y.pre ≫ a ≫ b
  hom := c
  post := y.post
  factor := by
    have hfactor := y.factor
    rw [← hy] at hfactor
    simpa [Category.assoc] using hfactor

/--
`x` starts and ends inside `y` when `x` has a three-part factorization whose
first and last located parts are both contained in `y`.
-/
def EndpointsIn (x y : Sub whole) : Prop :=
  ∃ (K : C) (L : C)
    (start : x.leftObj ⟶ K)
    (middle : K ⟶ L)
    (finish : L ⟶ x.rightObj)
    (factor : start ≫ middle ≫ finish = x.hom),
      leftPart x start middle finish factor ≤ y ∧
      rightPart x start middle finish factor ≤ y

end Sub

/--
The middle morphism of a located subpath is uniquely determined by its fixed
outer contexts inside `whole`.
-/
theorem middle_unique
    {C : Type u} [Category.{v} C] [IsPathCategory C]
    {A B : C}
    {whole : A ⟶ B}
    (s : Sub whole)
    (m : s.leftObj ⟶ s.rightObj)
    (h : s.pre ≫ m ≫ s.post = whole) :
    m = s.hom := by
  have hcontext :
      s.pre ≫ (m ≫ s.post) =
        s.pre ≫ (s.hom ≫ s.post) := by
    simpa [Category.assoc] using h.trans s.factor.symm
  have hright : m ≫ s.post = s.hom ≫ s.post :=
    IsPathCategory.left_cancel s.pre hcontext
  exact IsPathCategory.right_cancel s.post hright

/-- Convexity stated against an explicit three-part factorization of `x`. -/
theorem subpath_convex_parts
    {C : Type u} [Category.{v} C] [IsPathCategory C]
    {A B : C}
    {whole : A ⟶ B}
    (x y : Sub whole)
    {K L : C}
    (a : x.leftObj ⟶ K)
    (b : K ⟶ L)
    (c : L ⟶ x.rightObj)
    (hx : a ≫ b ≫ c = x.hom)
    (ha : Sub.leftPart x a b c hx ≤ y)
    (hc : Sub.rightPart x a b c hx ≤ y) :
    x ≤ y := by
  rcases ha with ⟨leftData⟩
  rcases hc with ⟨rightData⟩
  refine ⟨{
    left := leftData.left
    right := rightData.right
    middle_factor := ?_
    pre_factor := leftData.pre_factor
    post_factor := rightData.post_factor
  }⟩
  let m : y.leftObj ⟶ y.rightObj :=
    leftData.left ≫ x.hom ≫ rightData.right
  have hm : m = y.hom := by
    apply middle_unique y m
    have hpre : y.pre ≫ leftData.left = x.pre := by
      simpa [Sub.leftPart] using leftData.pre_factor
    have hpost : rightData.right ≫ y.post = x.post := by
      simpa [Sub.rightPart] using rightData.post_factor
    calc
      y.pre ≫ m ≫ y.post =
          (y.pre ≫ leftData.left) ≫ x.hom ≫
            (rightData.right ≫ y.post) := by
            simp [m, Category.assoc]
      _ = x.pre ≫ x.hom ≫ (rightData.right ≫ y.post) := by
            exact congrArg
              (fun pre => pre ≫ x.hom ≫ (rightData.right ≫ y.post))
              hpre
      _ = x.pre ≫ x.hom ≫ x.post := by
            exact congrArg (fun post => x.pre ≫ x.hom ≫ post) hpost
      _ = whole := x.factor
  simpa [m] using hm

/--
Convexity of located subpaths in a path category.

If a subpath starts and ends inside another subpath, then the whole subpath is
inside it.
-/
theorem subpath_convex
    {C : Type u} [Category.{v} C] [IsPathCategory C]
    {A B : C}
    {whole : A ⟶ B}
    {x y : Sub whole}
    (h : Sub.EndpointsIn x y) :
    x ≤ y := by
  rcases h with ⟨K, L, start, middle, finish, factor, start_le, finish_le⟩
  exact subpath_convex_parts x y start middle finish factor start_le finish_le

end Formal.Categorical.PathCategory
