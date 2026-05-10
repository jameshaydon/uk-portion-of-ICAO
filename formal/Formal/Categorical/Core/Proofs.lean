import Formal.Categorical.Core

namespace Formal.Categorical

universe u v

namespace FreeFinitePlanSystem

variable {Point : Type u}

theorem identity_not_leg
    (S : PlanSystem Point)
    {a b : Point}
    {p : S.Plan a b}
    (hp_id : IsIdPlan p)
    (hp_leg : S.IsLeg p) : False := by
  grind [S.leg_length_positive hp_leg, S.id_iff_length_zero p]

theorem front_rest_shorter
    (S : PlanSystem Point)
    {a b : Point}
    {p : S.Plan a b}
    (d : FrontDecomposition S.cat S.IsLeg p) :
    S.length d.rest < S.length p := by
  grind [S.leg_length_positive d.first_isLeg, S.comp_length, d.factor]

theorem back_init_shorter
    (S : PlanSystem Point)
    {a b : Point}
    {p : S.Plan a b}
    (d : BackDecomposition S.cat S.IsLeg p) :
    S.length d.init < S.length p := by
  grind [S.leg_length_positive d.last_isLeg, S.comp_length, d.factor]

theorem comp_eq_id_left_identity
    (S : PlanSystem Point)
    {a b : Point}
    (p : S.Plan a b)
    (q : S.Plan b a)
    (h : p ≫ q = S.cat.id a) :
    ∃ hendpoint : b = a, hendpoint ▸ p = S.cat.id a := by
  have hid : IsIdPlan (S.cat.id a) := ⟨rfl, rfl⟩
  have hcomp_zero : S.length (p ≫ q) = 0 := by
    grind [S.id_iff_length_zero]
  have hp_zero : S.length p = 0 := by
    grind [S.comp_length p q]
  exact (S.id_iff_length_zero p).2 hp_zero

theorem subplan_le_antisymm
    (S : PlanSystem Point)
    {a b : Point}
    {whole : S.Plan a b}
    {x y : Subplan S.cat whole}
    (hxy : x <= y)
    (hyx : y <= x) : x = y := by
  cases x with
  | mk xi xj xpre xmiddle xpost xfactor =>
  cases y with
  | mk yi yj ypre ymiddle ypost yfactor =>
  rcases hxy with ⟨xy⟩
  rcases hyx with ⟨yx⟩
  have left_x : yx.left ≫ xy.left = S.cat.id xi := by
    apply S.left_cancel xpre
    grind [yx.pre_factor, xy.pre_factor]
  have right_y : yx.right ≫ xy.right = S.cat.id yj := by
    apply S.right_cancel ypost
    grind [yx.post_factor, xy.post_factor]
  rcases S.comp_eq_id_left_identity yx.left xy.left left_x with
    ⟨hi, hyx_left_id⟩
  rcases S.comp_eq_id_left_identity yx.right xy.right right_y with
    ⟨hj, hyx_right_id⟩
  cases hi
  cases hj
  grind [yx.pre_factor, yx.middle_factor, yx.post_factor]

instance instSubplanPartialOrder
    (S : PlanSystem Point)
    {a b : Point}
    {whole : S.Plan a b} : PartialOrder (Subplan S.cat whole) :=
  { le := (· <= ·)
    lt := fun x y => x <= y ∧ ¬ y <= x
    le_refl := by
      intro s
      exact ⟨{
        left := S.cat.id s.i
        right := S.cat.id s.j
        middle_factor := by grind
        pre_factor := by grind
        post_factor := by grind
      }⟩
    le_trans := fun _ _ _ => Subplan.le_trans
    le_antisymm := fun _ _ => subplan_le_antisymm S
    lt_iff_le_not_ge := fun _ _ => Iff.rfl }

theorem identity_subplan_le_of_le
    (S : PlanSystem Point)
    {a b : Point}
    {whole : S.Plan a b}
    {current atom : Subplan S.cat whole}
    (hcurrent : IsIdPlan current.middle)
    (hle : atom <= current) :
    current <= atom := by
  cases current with
  | mk ci cj cpre cmid cpost cfactor =>
  cases atom with
  | mk ai aj apre amid apost afactor =>
  rcases hle with ⟨le⟩
  have current_len_zero : S.length cmid = 0 :=
    (S.id_iff_length_zero cmid).1 hcurrent
  rcases (S.id_iff_length_zero le.left).2
      (by grind [S.comp_length, le.middle_factor]) with
    ⟨hi, hleft_id⟩
  rcases (S.id_iff_length_zero le.right).2
      (by grind [S.comp_length, le.middle_factor]) with
    ⟨hj, hright_id⟩
  cases hi
  cases hj
  refine ⟨{
    left := S.cat.id ci
    right := S.cat.id cj
    middle_factor := ?_
    pre_factor := ?_
    post_factor := ?_
  }⟩ <;> grind [le.middle_factor, le.pre_factor, le.post_factor]

end FreeFinitePlanSystem

end Formal.Categorical
