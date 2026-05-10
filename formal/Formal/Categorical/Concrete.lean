import Formal.Categorical.Model
import Formal.Categorical.Extraction
import Formal.Categorical.Reconciliation
import Formal.Categorical.Reconciliation.FrontRecursive
import Formal.Categorical.Implementation
import Mathlib.Data.List.Zip

namespace Formal.Categorical

universe u

namespace Model.Concrete

open Extraction
open Reconciliation

/--
Map the route annotation on every concrete endpoint-indexed leg.

This is the endpoint-indexed version of `mapRoutes` in `app/Main.hs`.
-/
def mapRoutes
    {Point : Type u}
    {Route₁ Route₂ : Point -> Point -> Type u}
    (f : {a b : Point} -> Route₁ a b -> Route₂ a b) :
    {a b : Point} ->
      Plan Point Route₁ a b -> Plan Point Route₂ a b
  | _, _, Plan.nil point => Plan.nil point
  | _, _, Plan.cons here rest =>
      Plan.cons (f here) (mapRoutes f rest)

@[simp] theorem mapRoutes_nil
    {Point : Type u}
    {Route₁ Route₂ : Point -> Point -> Type u}
    (f : {a b : Point} -> Route₁ a b -> Route₂ a b)
    (point : Point) :
    @mapRoutes Point Route₁ Route₂ f point point
        (Plan.nil (Route := Route₁) point) =
      Plan.nil (Route := Route₂) point :=
  rfl

@[simp] theorem mapRoutes_cons
    {Point : Type u}
    {Route₁ Route₂ : Point -> Point -> Type u}
    (f : {a b : Point} -> Route₁ a b -> Route₂ a b)
    {a b c : Point}
    (here : Route₁ a b)
    (rest : Plan Point Route₁ b c) :
    @mapRoutes Point Route₁ Route₂ f a c (Plan.cons here rest) =
      Plan.cons (f here) (@mapRoutes Point Route₁ Route₂ f b c rest) :=
  rfl

@[simp] theorem mapRoutes_append
    {Point : Type u}
    {Route₁ Route₂ : Point -> Point -> Type u}
    (f : {a b : Point} -> Route₁ a b -> Route₂ a b)
    {a b c : Point}
    (left : Plan Point Route₁ a b)
    (right : Plan Point Route₁ b c) :
    @mapRoutes Point Route₁ Route₂ f a c (Plan.append left right) =
      Plan.append
        (@mapRoutes Point Route₁ Route₂ f a b left)
        (@mapRoutes Point Route₁ Route₂ f b c right) := by
  induction left with
  | nil point =>
      rfl
  | cons here rest ih =>
      exact congrArg (Plan.cons (f here)) (ih right)

/-- ICAO plans are concrete plans whose leg labels are ICAO route labels. -/
abbrev ICAO
    (Point : Type u)
    (Route : Point -> Point -> Type u)
    (a b : Point) : Type u :=
  Plan Point Route a b

/--
ADEXP plans are just waypoint paths.

The route annotation is intentionally trivial: all information is in the
endpoints of the legs.  This is the endpoint-indexed version of a raw ADEXP
waypoint stream.
-/
inductive ADEXPRoute (Point : Type u) : Point -> Point -> Type u where
  | empty {a b : Point} : ADEXPRoute Point a b

/-- ADEXP plans as concrete endpoint-indexed plans. -/
abbrev ADEXP
    (Point : Type u)
    (a b : Point) : Type u :=
  Plan Point (ADEXPRoute Point) a b

/--
Combined leg data: the ICAO route label and the ADEXP points reconciled
through that ICAO leg.
-/
structure Via
    (Point : Type u)
    (Route : Point -> Point -> Type u)
    (a b : Point) where
  route : Route a b
  through : List Point

/-- Combined plans as concrete endpoint-indexed plans. -/
abbrev Combined
    (Point : Type u)
    (Route : Point -> Point -> Type u)
    (a b : Point) : Type u :=
  Plan Point (Via Point Route) a b

/-- Projection from a combined plan to its ICAO route-labelled plan. -/
def projectICAO
    {Point : Type u}
    {Route : Point -> Point -> Type u}
    {a b : Point} :
    Combined Point Route a b -> ICAO Point Route a b :=
  mapRoutes (fun via => via.route)

private theorem projectICAO_length
    {Point : Type u}
    {Route : Point -> Point -> Type u}
    {a b : Point}
    (combined : Combined Point Route a b) :
    Plan.length (projectICAO combined) = Plan.length combined := by
  induction combined with
  | nil point =>
      rfl
  | cons here rest ih =>
      simpa [projectICAO] using congrArg (fun n => n + 1) ih

/-- The ADEXP path for one combined leg. -/
def adexpLegPath
    {Point : Type u}
    {a b : Point} :
    List Point -> ADEXP Point a b
  | [] =>
      Plan.cons (start := a) (mid := b) (finish := b)
        ADEXPRoute.empty
        (Plan.nil b)
  | point :: rest =>
      Plan.cons (start := a) (mid := point) (finish := b)
        ADEXPRoute.empty
        (adexpLegPath (a := point) (b := b) rest)

/--
Projection from a combined plan to its ADEXP waypoint path.

Unlike `projectICAO`, this is not one-leg-to-one-leg: each combined ICAO leg
expands to the ADEXP path through all reconciled waypoints.
-/
def projectADEXP {Point : Type u}
    {Route : Point -> Point -> Type u} :
    {a b : Point} ->
      Combined Point Route a b -> ADEXP Point a b
  | _, _, Plan.nil point =>
      Plan.nil point
  | start, _, Plan.cons (mid := mid) via rest =>
      Plan.append (adexpLegPath (a := start) (b := mid) via.through)
        (projectADEXP rest)

@[simp] theorem projectADEXP_nil
    {Point : Type u}
    {Route : Point -> Point -> Type u}
    (point : Point) :
    projectADEXP (Plan.nil (Route := Via Point Route) point) =
        Plan.nil (Route := ADEXPRoute Point) point :=
  rfl

@[simp] theorem projectADEXP_cons
    {Point : Type u}
    {Route : Point -> Point -> Type u}
    {a b c : Point}
    (via : Via Point Route a b)
    (rest : Combined Point Route b c) :
    projectADEXP (Plan.cons via rest) =
      Plan.append (adexpLegPath (a := a) (b := b) via.through)
        (projectADEXP rest) :=
  rfl

theorem projectADEXP_append
    {Point : Type u}
    {Route : Point -> Point -> Type u}
    {a b c : Point}
    (left : Combined Point Route a b)
    (right : Combined Point Route b c) :
    projectADEXP (Plan.append left right) =
      Plan.append (projectADEXP left) (projectADEXP right) := by
  induction left with
  | nil point =>
      rfl
  | cons via rest ih =>
      simp [Plan.append, projectADEXP_cons, ih, Plan.append_assoc]

/-- All waypoints after the start point of an ADEXP path. -/
def adexpTail
    {Point : Type u} :
    {a b : Point} ->
      ADEXP Point a b -> List Point
  | _, _, Plan.nil _ =>
      []
  | _, _, Plan.cons (mid := mid) _ rest =>
      mid :: adexpTail rest

/-- The raw waypoint stream represented by an ADEXP path. -/
def adexpWaypoints
    {Point : Type u} :
    {a b : Point} ->
      ADEXP Point a b -> List Point
  | start, _, plan =>
      start :: adexpTail plan

@[simp] theorem adexpTail_append
    {Point : Type u}
    {a b c : Point}
    (left : ADEXP Point a b)
    (right : ADEXP Point b c) :
    adexpTail (Plan.append left right) =
      adexpTail left ++ adexpTail right := by
  induction left with
  | nil point =>
      rfl
  | cons here rest ih =>
      simp [Plan.append, adexpTail, ih]

@[simp] theorem adexpWaypoints_append
    {Point : Type u}
    {a b c : Point}
    (left : ADEXP Point a b)
    (right : ADEXP Point b c) :
    adexpWaypoints (Plan.append left right) =
      a :: adexpTail left ++ adexpTail right := by
  simp [adexpWaypoints]

@[simp] theorem adexpTail_adexpLegPath
    {Point : Type u}
    {a b : Point}
    (through : List Point) :
    adexpTail (adexpLegPath (Point := Point) (a := a) (b := b) through) =
      through ++ [b] := by
  induction through generalizing a with
  | nil =>
      simp [adexpLegPath, adexpTail]
  | cons point rest ih =>
      simp [adexpLegPath, adexpTail, ih]

/-- All waypoints after the start point of a combined path's ADEXP projection. -/
def combinedTail
    {Point : Type u}
    {Route : Point -> Point -> Type u} :
    {a b : Point} ->
      Combined Point Route a b -> List Point
  | _, _, Plan.nil _ =>
      []
  | _, _, Plan.cons (mid := mid) via rest =>
      via.through ++ mid :: combinedTail rest

/-- The raw ADEXP waypoint stream represented by a combined plan. -/
def combinedWaypoints
    {Point : Type u}
    {Route : Point -> Point -> Type u} :
    {a b : Point} ->
      Combined Point Route a b -> List Point
  | start, _, plan =>
      start :: combinedTail plan

@[simp] theorem adexpTail_projectADEXP
    {Point : Type u}
    {Route : Point -> Point -> Type u}
    {a b : Point}
    (combined : Combined Point Route a b) :
    adexpTail (projectADEXP combined) =
      combinedTail combined := by
  induction combined with
  | nil point =>
      rfl
  | cons via rest ih =>
      simp [projectADEXP_cons, combinedTail, ih, List.append_assoc]

theorem adexpWaypoints_projectADEXP
    {Point : Type u}
    {Route : Point -> Point -> Type u}
    {a b : Point}
    (combined : Combined Point Route a b) :
    adexpWaypoints (projectADEXP combined) =
      combinedWaypoints combined := by
  cases combined with
  | nil point =>
      rfl
  | cons via rest =>
      simp [adexpWaypoints, adexpTail_projectADEXP, combinedWaypoints,
        combinedTail]

theorem adexp_eq_of_tail_eq
    {Point : Type u}
    {a b : Point}
    {left right : ADEXP Point a b}
    (h : adexpTail left = adexpTail right) :
    left = right := by
  induction left with
  | nil point =>
      cases right with
      | nil _ =>
          rfl
      | cons here rest =>
          simp [adexpTail] at h
  | cons here leftRest ih =>
      cases right with
      | nil _ =>
          simp [adexpTail] at h
      | cons there rightRest =>
          simp [adexpTail] at h
          rcases h with ⟨hmid, htail⟩
          cases hmid
          cases here
          cases there
          exact congrArg (Plan.cons ADEXPRoute.empty) (ih htail)

/-- Concrete ICAO/ADEXP/Combined reconciliation system over `Model.Plan`. -/
def reconciliationSystem
    (Point : Type u)
    (Route : Point -> Point -> Type u) :
    ReconciliationSystem.{u, u} where
  Point := Point
  ICAO := Plan.freeFinitePlanSystem Point Route
  ADEXP := Plan.freeFinitePlanSystem Point (ADEXPRoute Point)
  Combined := Plan.freeFinitePlanSystem Point (Via Point Route)
  toIcao :=
    { map := fun p => projectICAO p
      map_id := by
        intro point
        rfl
      map_comp := by
        intro a b c left right
        exact mapRoutes_append (fun via => via.route) left right }
  toAdexp :=
    { map := fun p => projectADEXP p
      map_id := by
        intro point
        rfl
      map_comp := by
        intro a b c left right
        exact projectADEXP_append left right }

namespace UK

variable {Point : Type u}
variable {Route : Point -> Point -> Type u}

/--
Closed concrete UK relevance for one combined leg.

A leg is UK-relevant when its start waypoint, one reconciled through-point, or
its end waypoint is UK-relevant.
-/
def ViaHasUK
    (uk : Point -> Prop)
    {a b : Point}
    (via : Via Point Route a b) : Prop :=
  uk a ∨ (∃ p : Point, p ∈ via.through ∧ uk p) ∨ uk b

@[reducible] private def combinedSystem
    {Point : Type u}
    (Route : Point -> Point -> Type u) :
    FreeFinitePlanSystem.{u, u} Point :=
  (reconciliationSystem Point Route).Combined

@[reducible] private def singleVia {a b : Point}
    (via : Via Point Route a b) :
    (combinedSystem Route).Plan a b :=
  Plan.single via

/-- A concrete point occurrence is UK-relevant when its waypoint is UK. -/
def pointHasUK
    (uk : Point -> Prop)
    {a b : Point}
    {whole : (combinedSystem Route).Plan a b}
    (point : PointOccurrence (combinedSystem Route) whole) : Prop :=
  uk point.val.i

def rawLegHasUK (uk : Point -> Prop)
    {a b : Point}
    {whole : (combinedSystem Route).Plan a b}
    (leg : LegOccurrence (combinedSystem Route) whole) : Prop :=
  ∃ via : Via Point Route leg.val.i leg.val.j,
    leg.val.middle = singleVia via ∧
      ViaHasUK uk via

private theorem point_inside_leg_rawLegHasUK
    (uk : Point -> Prop)
    {a b : Point}
    {whole : (combinedSystem Route).Plan a b}
    (point : PointOccurrence (combinedSystem Route) whole)
    (leg : LegOccurrence (combinedSystem Route) whole)
    (hpoint : pointHasUK uk point)
    (hle : point.val <= leg.val) :
    rawLegHasUK uk leg := by
  unfold rawLegHasUK
  rcases leg.property with ⟨via, hvia⟩
  refine ⟨via, hvia, ?_⟩
  unfold pointHasUK at hpoint
  change
    uk leg.val.i ∨ (∃ p : Point, p ∈ via.through ∧ uk p) ∨
      uk leg.val.j
  rcases hle with ⟨data⟩
  have hpoint_len :
      Plan.length point.val.middle = 0 :=
    (Plan.isIdPlan_iff_length_zero
      (Route := Via Point Route) point.val.middle).1 point.property
  have hlen := congrArg Plan.length (data.middle_factor.trans hvia)
  change
      Plan.length
        (Plan.append (Plan.append data.left point.val.middle) data.right) =
      Plan.length (singleVia via) at hlen
  simp [Plan.length_append, hpoint_len, singleVia, Plan.single, Plan.length]
    at hlen
  by_cases hleft : Plan.length data.left = 0
  · have left_id :
        IsIdPlan (C := Plan.category Point (Via Point Route)) data.left :=
      (Plan.isIdPlan_iff_length_zero
        (Route := Via Point Route) data.left).2 hleft
    rcases left_id with ⟨hendpoint, _⟩
    exact Or.inl (hendpoint ▸ hpoint)
  · have hright : Plan.length data.right = 0 := by
      omega
    have point_id := point.property
    have right_id :
        IsIdPlan (C := Plan.category Point (Via Point Route)) data.right :=
      (Plan.isIdPlan_iff_length_zero
        (Route := Via Point Route) data.right).2 hright
    rcases point_id with ⟨hpoint_endpoint, _⟩
    rcases right_id with ⟨hright_endpoint, _⟩
    have hend : leg.val.j = point.val.i :=
      hright_endpoint.trans hpoint_endpoint
    exact Or.inr (Or.inr (hend.symm ▸ hpoint))

private theorem single_via_factor_hasUK
    (uk : Point -> Prop)
    {outerStart innerStart innerFinish outerFinish : Point}
    (left : Plan Point (Via Point Route) outerStart innerStart)
    (right : Plan Point (Via Point Route) innerFinish outerFinish)
    (innerVia : Via Point Route innerStart innerFinish)
    (outerVia : Via Point Route outerStart outerFinish)
    (hfactor :
      Plan.append (Plan.append left (singleVia innerVia))
          right =
        singleVia outerVia)
    (hinnerUK : ViaHasUK uk innerVia) :
    ViaHasUK uk outerVia := by
  cases left with
  | nil _ =>
      cases right with
      | nil _ =>
          simp [singleVia, Plan.single, Plan.append] at hfactor
          cases hfactor
          exact hinnerUK
      | cons rightVia rightRest =>
          have hlen := congrArg Plan.length hfactor
          simp [singleVia, Plan.single, Plan.append, Plan.length] at hlen
  | cons leftVia leftRest =>
      have hlen := congrArg Plan.length hfactor
      simp [singleVia, Plan.single, Plan.append, Plan.length,
        Plan.length_append] at hlen

private theorem leg_inside_leg_rawLegHasUK
    (uk : Point -> Prop)
    {a b : Point}
    {whole : (combinedSystem Route).Plan a b}
    (inner : LegOccurrence (combinedSystem Route) whole)
    (outer : LegOccurrence (combinedSystem Route) whole)
    (hinner : rawLegHasUK uk inner)
    (hle : inner.val <= outer.val) :
    rawLegHasUK uk outer := by
  unfold rawLegHasUK at hinner ⊢
  rcases outer.property with ⟨outerVia, houterVia⟩
  refine ⟨outerVia, houterVia, ?_⟩
  rcases hinner with ⟨innerVia, hinnerVia, hinnerUK⟩
  rcases hle with ⟨data⟩
  have hfactor :
      Plan.append
          (Plan.append data.left (singleVia innerVia))
          data.right =
        singleVia outerVia := by
    simpa [hinnerVia, houterVia, singleVia] using data.middle_factor
  exact single_via_factor_hasUK uk
    data.left data.right innerVia outerVia hfactor hinnerUK

/--
Executable concrete relevance: points are checked directly and legs are
checked by their own `Via` data. The closure law follows from the concrete
one-leg containment lemmas above.
-/
def rawRelevance (uk : Point -> Prop) :
    UKRelevance (combinedSystem Route) where
  pointHasUK := pointHasUK uk
  legHasUK := rawLegHasUK uk
  legHasUK_of_atom_le := by
    intro a b whole atom leg atom_uk hle
    rcases atom_uk with ⟨hatom_point, hpoint⟩ | ⟨hatom_leg, hleg⟩
    · exact point_inside_leg_rawLegHasUK uk
        ⟨atom.subplan, hatom_point⟩ leg hpoint hle
    · exact leg_inside_leg_rawLegHasUK uk
        ⟨atom.subplan, hatom_leg⟩ leg hleg hle

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
      | isFalse hpoint, isTrue hrest =>
          isTrue (by
            rcases hrest with ⟨p, hp, huk⟩
            exact ⟨p, List.Mem.tail _ hp, huk⟩)
      | isFalse hpoint, isFalse hrest =>
          isFalse (by aesop)

def viaHasUKDecidable (uk : Point -> Prop)
    [DecidablePred uk]
    {a b : Point}
    (via : Via Point Route a b) :
    Decidable (ViaHasUK uk via) :=
  match inferInstanceAs (Decidable (uk a)),
    throughHasUKDecidable (Point := Point) uk via.through,
    inferInstanceAs (Decidable (uk b)) with
  | isTrue ha, _, _ =>
      isTrue (Or.inl ha)
  | isFalse _, isTrue hthrough, _ =>
      isTrue (Or.inr (Or.inl hthrough))
  | isFalse _, isFalse _, isTrue hb =>
      isTrue (Or.inr (Or.inr hb))
  | isFalse hna, isFalse hnthrough, isFalse hnb =>
      isFalse (by
        intro h
        rcases h with ha | hthrough | hb
        · exact hna ha
        · exact hnthrough hthrough
        · exact hnb hb)

def atomHasUKDecidable
    (uk : Point -> Prop)
    [DecidablePred uk]
    {a b : Point}
    {whole : (combinedSystem Route).Plan a b}
    (atom : AtomicOccurrence (combinedSystem Route) whole) :
    Decidable
      (AtomHasUK
        (rawRelevance uk)
        atom) := by
  rcases atom with ⟨subplan, hatom⟩
  rcases subplan with ⟨i, j, pre, middle, post, factor⟩
  unfold AtomHasUK AtomicOccurrenceHasUK rawRelevance pointHasUK rawLegHasUK
  cases middle with
  | nil point =>
      cases (inferInstance : Decidable (uk i)) with
      | isTrue h =>
          exact isTrue (Or.inl ⟨⟨rfl, rfl⟩, h⟩)
      | isFalse hnot =>
          refine isFalse ?_
          intro h
          rcases h with ⟨_, hpoint⟩ | ⟨hleg, _⟩
          · exact hnot hpoint
          · rcases hleg with ⟨route, hroute⟩
            have hlen := congrArg Plan.length hroute
            simp [Plan.single, Plan.length] at hlen
  | cons via rest =>
      cases rest with
      | nil _ =>
          cases viaHasUKDecidable uk via with
          | isTrue hvia =>
              exact isTrue (Or.inr ⟨⟨via, rfl⟩, via, rfl, hvia⟩)
          | isFalse hvia_not =>
              refine isFalse ?_
              intro h
              rcases h with ⟨hid, _⟩ | ⟨_, hraw⟩
              · have hlen :
                    Plan.length (Plan.cons via (Plan.nil j)) = 0 :=
                  (Plan.isIdPlan_iff_length_zero
                    (Route := Via Point Route)
                    (Plan.cons via (Plan.nil j))).1 hid
                simp [Plan.length] at hlen
              · rcases hraw with ⟨via', hmiddle, hvia'⟩
                cases hmiddle
                exact hvia_not hvia'
      | cons next tail =>
          exact False.elim (by
            rcases hatom with hid | hleg
            · have hlen :
                  Plan.length (Plan.cons via (Plan.cons next tail)) = 0 :=
                (Plan.isIdPlan_iff_length_zero
                  (Route := Via Point Route)
                  (Plan.cons via (Plan.cons next tail))).1 hid
              simp [Plan.length] at hlen
            · rcases hleg with ⟨route, hroute⟩
              have hlen := congrArg Plan.length hroute
              simp [Plan.single, Plan.length] at hlen)

private def firstLegAtom {a b c : Point}
    (via : Via Point Route a b)
    (rest : (combinedSystem Route).Plan b c) :
    AtomicOccurrence (combinedSystem Route) (Plan.cons via rest) where
  subplan :=
    { i := a
      j := b
      pre := Plan.nil a
      middle := singleVia via
      post := rest
      factor := rfl }
  isAtomic := Or.inr ⟨via, rfl⟩

private def tailAtom {a b c : Point}
    (via : Via Point Route a b)
    {rest : (combinedSystem Route).Plan b c}
    (atom : AtomicOccurrence (combinedSystem Route) rest) :
    AtomicOccurrence (combinedSystem Route) (Plan.cons via rest) where
  subplan :=
    { i := atom.subplan.i
      j := atom.subplan.j
      pre := Plan.cons via atom.subplan.pre
      middle := atom.subplan.middle
      post := atom.subplan.post
      factor := by
        change
          Plan.cons via
              (Plan.append
                (Plan.append atom.subplan.pre atom.subplan.middle)
                atom.subplan.post) =
            Plan.cons via rest
        exact congrArg (Plan.cons via) (by simpa using atom.subplan.factor) }
  isAtomic := atom.isAtomic

private theorem tailAtom_hasUK (uk : Point -> Prop)
    {a b c : Point}
    (via : Via Point Route a b)
    {rest : (combinedSystem Route).Plan b c}
    (atom : AtomicOccurrence (combinedSystem Route) rest) :
    AtomHasUK (rawRelevance uk) atom ->
      AtomHasUK
        (rawRelevance uk)
        (tailAtom via atom) := by
  intro h
  simpa [tailAtom, rawRelevance, AtomHasUK, AtomicOccurrenceHasUK,
    pointHasUK, rawLegHasUK] using h

private def frontOfCons {a b c : Point}
    (via : Via Point Route a b)
    (rest : (combinedSystem Route).Plan b c) :
    FrontDecomposition
      (combinedSystem Route).cat
      (combinedSystem Route).IsLeg
      (Plan.cons via rest) where
  cut := b
  first := singleVia via
  rest := rest
  first_isLeg := ⟨via, rfl⟩
  factor := rfl

private theorem tailAtom_reflects_hasUK
    (uk : Point -> Prop)
    {a b c : Point}
    (via : Via Point Route a b)
    {rest : (combinedSystem Route).Plan b c}
    (atom : AtomicOccurrence (combinedSystem Route) (Plan.cons via rest))
    (hle :
      atom.subplan <=
        frontPeel
          (Subplan.whole (combinedSystem Route).cat (Plan.cons via rest))
          (frontOfCons via rest)) :
    AtomHasUK (rawRelevance uk) atom ->
      HasUK (rawRelevance uk) rest := by
  intro h
  rcases hle with ⟨data⟩
  let localAtom : AtomicOccurrence (combinedSystem Route) rest :=
    { subplan :=
        { i := atom.subplan.i
          j := atom.subplan.j
          pre := data.left
          middle := atom.subplan.middle
          post := data.right
          factor := by
            simpa [frontPeel, Subplan.whole, singleVia]
              using data.middle_factor }
      isAtomic := atom.isAtomic }
  refine ⟨localAtom, ?_⟩
  simpa [localAtom, rawRelevance, AtomHasUK, AtomicOccurrenceHasUK,
    pointHasUK, rawLegHasUK] using h

private theorem hasUK_iff_first_or_rest
    (uk : Point -> Prop)
    {a b c : Point}
    (via : Via Point Route a b)
    (rest : (combinedSystem Route).Plan b c) :
    HasUK (rawRelevance uk) (Plan.cons via rest) ↔
      ViaHasUK uk via ∨ HasUK (rawRelevance uk) rest := by
  constructor
  · intro h
    rcases h with ⟨atom, hatom⟩
    let current :=
      Subplan.whole (combinedSystem Route).cat (Plan.cons via rest)
    let front :
        FrontDecomposition
          (combinedSystem Route).cat
          (combinedSystem Route).IsLeg
          current.middle :=
      frontOfCons via rest
    have hloc :=
      (combinedSystem Route).front_atom_le_boundary_or_remainder
        current front atom.subplan atom.isAtomic
        (Subplan.le_whole atom.subplan)
    rcases hloc with hboundary | hremainder
    · left
      let leg : LegOccurrence
          (combinedSystem Route) (Plan.cons via rest) :=
        ⟨frontHead current front, front.first_isLeg⟩
      have hraw :
          rawLegHasUK uk leg :=
        (rawRelevance uk).legHasUK_of_atom_le
          atom leg hatom hboundary
      rcases hraw with ⟨via', hmiddle, hvia'⟩
      cases hmiddle
      exact hvia'
    · right
      exact tailAtom_reflects_hasUK uk via atom hremainder hatom
  · intro h
    rcases h with hvia | hrest
    · refine ⟨firstLegAtom via rest, ?_⟩
      exact Or.inr ⟨⟨via, rfl⟩, via, rfl, hvia⟩
    · rcases hrest with ⟨atom, hatom⟩
      exact ⟨tailAtom via atom, tailAtom_hasUK uk via atom hatom⟩

private theorem noUK_nil (uk : Point -> Prop)
    {point : Point}
    (hnot : ¬ uk point) :
    ¬ HasUK
      (rawRelevance uk)
      (Plan.nil (Route := Via Point Route) point) := by
  intro h
  rcases h with ⟨atom, hatom⟩
  rcases atom with ⟨subplan, hatomic⟩
  rcases subplan with ⟨i, j, pre, middle, post, factor⟩
  have hlen := congrArg Plan.length factor
  change
    Plan.length (Plan.append (Plan.append pre middle) post) =
      Plan.length (Plan.nil (Route := Via Point Route) point) at hlen
  simp [Plan.length_append, Plan.length] at hlen
  have hpre_zero : Plan.length pre = 0 := by omega
  have hmiddle_zero : Plan.length middle = 0 := by omega
  unfold AtomHasUK AtomicOccurrenceHasUK rawRelevance pointHasUK rawLegHasUK at hatom
  rcases hatom with ⟨hpoint_subplan, hpoint⟩ | ⟨_hleg_subplan, hraw⟩
  · have hpre_id :
        IsIdPlan (C := Plan.category Point (Via Point Route)) pre :=
      (Plan.isIdPlan_iff_length_zero
        (Route := Via Point Route) pre).2 hpre_zero
    rcases hpre_id with ⟨hendpoint, _⟩
    exact hnot (hendpoint ▸ hpoint)
  · rcases hraw with ⟨via, hmiddle, _⟩
    have hmiddle' : middle = singleVia via := by
      simpa using hmiddle
    rw [hmiddle'] at hmiddle_zero
    simp [singleVia, Plan.single, Plan.length] at hmiddle_zero

def hasUKDecidable
    (uk : Point -> Prop)
    [DecidablePred uk] :
    {a b : Point} ->
      (whole : (combinedSystem Route).Plan a b) ->
        Decidable
          (HasUK
            (rawRelevance uk)
            whole)
  | _, _, Plan.nil point =>
      match inferInstanceAs (Decidable (uk point)) with
      | isTrue hpoint =>
          isTrue (by
            let current :=
              Subplan.whole (combinedSystem Route).cat
                (Plan.nil (Route := Via Point Route) point)
            refine ⟨identityOccurrence (combinedSystem Route)
              current ⟨rfl, rfl⟩, ?_⟩
            exact Or.inl ⟨⟨rfl, rfl⟩, hpoint⟩)
      | isFalse hpoint =>
          isFalse (noUK_nil uk hpoint)
  | _, _, Plan.cons via rest =>
      match viaHasUKDecidable uk via,
        hasUKDecidable uk rest with
      | isTrue hvia, _ =>
          isTrue ((hasUK_iff_first_or_rest uk via rest).2 (Or.inl hvia))
      | isFalse _, isTrue hrest =>
          isTrue ((hasUK_iff_first_or_rest uk via rest).2 (Or.inr hrest))
      | isFalse hnot_via, isFalse hnot_rest =>
          isFalse (by
            intro h
            rcases (hasUK_iff_first_or_rest uk via rest).1 h with
              hvia | hrest
            · exact hnot_via hvia
            · exact hnot_rest hrest)

end UK

section Implementation

/--
Concrete extraction by the proved front-then-back chopping loop.

This is specialized to the concrete executable relevance predicate, so it can
use direct `Decidable` procedures rather than an implementation that must work
for every abstract `UKRelevance`.
-/
def decideByConcreteChopping {Point : Type u}
    {Route : Point -> Point -> Type u}
    (uk : Point -> Prop)
    [DecidablePred uk]
    {a b : Point}
    (whole : Combined Point Route a b) :
    LeastUKSubplanDecision
      (reconciliationSystem Point Route).Combined
      (UK.rawRelevance uk)
      whole :=
  match UK.hasUKDecidable uk whole with
  | isFalse noUK =>
      LeastUKSubplanDecision.none noUK
  | isTrue hasUK =>
      let result :
          { selected :
              Subplan (reconciliationSystem Point Route).Combined.cat whole //
            CoversUK (UK.rawRelevance uk) selected ∧
              LeastUKSubplan (UK.rawRelevance uk) selected } :=
        trimFromWholeByChopping whole
          (UK.atomHasUKDecidable uk)
          hasUK
      LeastUKSubplanDecision.found result.val hasUK result.property.2

theorem decideByConcreteChopping_correct
    {Point : Type u}
    {Route : Point -> Point -> Type u}
    (uk : Point -> Prop)
    [DecidablePred uk]
    {a b : Point}
    (whole : Combined Point Route a b) :
    match decideByConcreteChopping uk whole with
    | LeastUKSubplanDecision.none _ =>
        ¬ HasUK
          (UK.rawRelevance uk)
          whole
    | LeastUKSubplanDecision.found selected _ _ =>
        HasUK
          (UK.rawRelevance uk)
          whole ∧
        LeastUKSubplan
          (UK.rawRelevance uk)
          selected := by
  unfold decideByConcreteChopping
  cases UK.hasUKDecidable uk whole with
  | isFalse noUK =>
      exact noUK
  | isTrue hasUK =>
      exact ⟨hasUK,
        (trimFromWholeByChopping whole
          (UK.atomHasUKDecidable uk)
          hasUK).property.2⟩

/-- Observable errors for the raw ADEXP executable path, matching `app/Main.hs`. -/
inductive RawErr where
  | nonUkPlan
  | cannotReconcileIcaoAdexp
  | ambiguousReconciliationsOfIcaoAdexp
deriving Repr, DecidableEq

/--
The raw executable result. A successful result carries the selected ICAO
portion with its own endpoints.
-/
inductive RawIcaoResult
    (Point : Type u)
    (Route : Point -> Point -> Type u) : Type u where
  | error : RawErr -> RawIcaoResult Point Route
  | ok :
      (start finish : Point) ->
      ICAO Point Route start finish ->
        RawIcaoResult Point Route

namespace Raw

variable {Point : Type u}
variable {Route : Point -> Point -> Type u}

/--
All ways to split a list into a prefix and suffix.

This is the executable Lean counterpart of the Haskell definition
`splits xs = zip (inits xs) (tails xs)`.
-/
def splits (points : List Point) : List (List Point × List Point) :=
  points.inits.zip points.tails

theorem mem_splits_iff
    {points : List Point}
    (split : List Point × List Point) :
    split ∈ splits (Point := Point) points ↔ split.1 ++ split.2 = points := by
  exact List.mem_zip_inits_tails

theorem splits_cons
    (point : Point)
    (rest : List Point) :
    splits (Point := Point) (point :: rest) =
      ([], point :: rest) ::
        (splits (Point := Point) rest).map
          (fun split => (point :: split.1, split.2)) := by
  simp [splits, List.zip_map_left]

theorem splits_nodup :
    {points : List Point} ->
      (splits (Point := Point) points).Nodup
  | [] => by
      simp [splits]
  | point :: rest => by
      rw [splits_cons]
      constructor
      · intro split hmem heq
        rcases List.mem_map.1 hmem with ⟨rawSplit, _, hrawSplit⟩
        cases hrawSplit
        simpa using congrArg Prod.fst heq
      · exact
        (splits_nodup (points := rest)).map
          (by
            intro left right heq
            cases left with
            | mk leftPrefix leftSuffix =>
                cases right with
                | mk rightPrefix rightSuffix =>
                    cases heq
                    rfl)

/--
Flatten a reconciled combined plan back to the raw ADEXP waypoint stream used
by `app/Main.hs`.
-/
def flattenCombined :
    {a b : Point} ->
      Combined Point Route a b ->
        List Point
  | _, _, combined =>
      combinedWaypoints combined

end Raw

/-- Convert a raw waypoint stream to the endpoint-indexed ADEXP path shape. -/
def adexpOfRaw? [DecidableEq Point] :
    {a b : Point} ->
      List Point -> Option (ADEXP Point a b)
  | a, b, [] =>
      none
  | a, b, [point] =>
      if hstart : a = point then
        if hfinish : b = point then
          by
            cases hstart
            cases hfinish
            exact some (Plan.nil (Route := ADEXPRoute Point) _)
        else
          none
      else
        none
  | a, b, point :: next :: rest =>
      if hstart : a = point then
        by
          cases hstart
          match adexpOfRaw? (Point := Point) (a := next) (b := b)
              (next :: rest) with
          | none =>
              exact none
          | some tail =>
              exact some (Plan.cons ADEXPRoute.empty tail)
      else
        none
termination_by _ _ raw => raw.length
decreasing_by
  simp_wf

theorem adexpOfRaw?_sound [DecidableEq Point]
    {a b : Point}
    {raw : List Point}
    {adexp : ADEXP Point a b}
    (h : adexpOfRaw? (Point := Point) (a := a) (b := b) raw = some adexp) :
    adexpWaypoints adexp = raw := by
  induction raw generalizing a with
  | nil =>
      simp [adexpOfRaw?] at h
  | cons point rest ih =>
      cases rest with
      | nil =>
          rw [adexpOfRaw?.eq_def] at h
          by_cases hstart : a = point
          · simp [hstart] at h
            by_cases hfinish : b = point
            · simp [hfinish] at h
              cases hstart
              cases hfinish
              cases h
              rfl
            · simp [hfinish] at h
          · simp [hstart] at h
      | cons next rest =>
          rw [adexpOfRaw?.eq_def] at h
          by_cases hstart : a = point
          · simp [hstart] at h
            cases hstart
            cases htail :
                adexpOfRaw? (Point := Point) (a := next) (b := b)
                  (next :: rest) with
            | none =>
                simp [htail] at h
            | some tail =>
                simp [htail] at h
                cases h
                have htail_raw := ih (a := next) htail
                simp [adexpWaypoints] at htail_raw
                simp [adexpWaypoints, adexpTail, htail_raw]
          · simp [hstart] at h

theorem adexpOfRaw?_complete [DecidableEq Point]
    {a b : Point}
    (adexp : ADEXP Point a b) :
    adexpOfRaw? (Point := Point) (a := a) (b := b)
      (adexpWaypoints adexp) = some adexp := by
  induction adexp with
  | nil point =>
      simp [adexpOfRaw?, adexpWaypoints, adexpTail]
  | cons here rest ih =>
      cases here
      have hrec := ih
      simp [adexpWaypoints] at hrec
      simp [adexpOfRaw?, adexpWaypoints, adexpTail, hrec]

namespace ReconciliationSearch

variable {Point : Type u}
variable {Route : Point -> Point -> Type u}

theorem adexpLegPath_inj
    {a b : Point}
    {left right : List Point}
    (h :
      adexpLegPath (Point := Point) (a := a) (b := b) left =
        adexpLegPath (Point := Point) (a := a) (b := b) right) :
    left = right := by
  have htail := congrArg adexpTail h
  exact List.append_left_injective [b]
    (by simpa [adexpTail_adexpLegPath] using htail)

theorem adexpLegPath_length_pos
    {a b : Point}
    (through : List Point) :
    0 <
      Plan.length
        (adexpLegPath (Point := Point) (a := a) (b := b) through) := by
  cases through with
  | nil =>
      simp [adexpLegPath, Plan.length]
  | cons point rest =>
      simp [adexpLegPath, Plan.length]

def legReconciliation {a b : Point}
    (icaoLeg :
      LegPlan (reconciliationSystem Point Route).ICAO (a := a) (b := b))
    (through : List Point) :
    LegReconciliation
      (reconciliationSystem Point Route)
      icaoLeg
      (adexpLegPath (Point := Point) (a := a) (b := b) through) := by
  rcases icaoLeg with ⟨icaoLeg, hicaoLeg⟩
  cases icaoLeg with
  | nil point =>
      have hpos := Plan.leg_length_positive hicaoLeg
      simp [Plan.length] at hpos
  | cons route rest =>
      cases rest with
      | nil _ =>
          refine
            { combinedLeg :=
                ⟨Plan.single ({ route := route, through := through } :
                  Via Point Route a b), ?_⟩
              toIcao_eq := ?_
              toAdexp_eq := ?_
              unique := ?_ }
          · exact ⟨{ route := route, through := through }, rfl⟩
          · rfl
          · change
              projectADEXP (Plan.cons
                    ({ route := route, through := through } :
                      Via Point Route a b)
                    (Plan.nil b)) =
                adexpLegPath through
            simp [projectADEXP]
          · intro other hicao hadexp
            rcases other with ⟨otherLeg, hotherLeg⟩
            cases otherLeg with
            | nil point =>
                have hpos := Plan.leg_length_positive hotherLeg
                simp [Plan.length] at hpos
            | cons via otherRest =>
                cases otherRest with
                | nil _ =>
                    have hroute : via.route = route := by
                      change
                        projectICAO (Plan.cons via (Plan.nil b)) =
                          Plan.cons route (Plan.nil b) at hicao
                      simpa [projectICAO, mapRoutes] using hicao
                    have hthrough : via.through = through := by
                      apply adexpLegPath_inj (Point := Point)
                      change
                        projectADEXP (Plan.cons via (Plan.nil b)) =
                          adexpLegPath through at hadexp
                      simpa [projectADEXP, Plan.append] using hadexp
                    cases via
                    simp at hroute hthrough
                    cases hroute
                    cases hthrough
                    rfl
                | cons next tail =>
                    have hlen := Plan.leg_length_eq_one hotherLeg
                    simp [Plan.length] at hlen
      | cons next tail =>
          have hlen := Plan.leg_length_eq_one hicaoLeg
          simp [Plan.length] at hlen

def identityBaseEnumerator :
    IdentityBaseEnumerator (reconciliationSystem Point Route) where
  candidates :=
    fun
      | Plan.nil point =>
          [Plan.nil (Route := Via Point Route) point]
      | Plan.cons _ _ =>
          []
  sound := by
    intro a b icao hicao_id adexp combined hmem
    cases adexp with
    | nil _ =>
        have hcombined : combined = Plan.nil (Route := Via Point Route) a := by
          cases hmem with
          | head =>
              rfl
          | tail _ htail =>
              cases htail
        cases hcombined
        rcases hicao_id with ⟨hend, hicao⟩
        cases hend
        cases hicao
        constructor <;> rfl
    | cons here rest =>
        cases hmem
  complete := by
    intro a b icao hicao_id adexp combined htoIcao htoAdexp
    have hcombined_id :
        IsIdPlan
          (C := Plan.category Point (Via Point Route))
          combined := by
      apply
        (Plan.isIdPlan_iff_length_zero
          (Route := Via Point Route) combined).2
      have hicao_len :
          Plan.length (projectICAO combined) = 0 := by
        change Plan.length ((reconciliationSystem Point Route).toIcao.map combined) = 0
        rw [htoIcao]
        exact
          (Plan.isIdPlan_iff_length_zero
            (Route := Route) icao).1 hicao_id
      simpa [projectICAO_length combined] using hicao_len
    rcases hcombined_id with ⟨hend, hcombined⟩
    cases hend
    cases hcombined
    change projectADEXP (Plan.nil a) = adexp at htoAdexp
    cases htoAdexp
    exact List.mem_singleton_self _
  nodup := by
    intro a b adexp
    cases adexp with
    | nil _ =>
        exact List.nodup_singleton _
    | cons here rest =>
        exact List.nodup_nil

def frontSplitOfRawSplit? [DecidableEq Point]
    {a m b : Point}
    (icaoLeg :
      LegPlan (reconciliationSystem Point Route).ICAO (a := a) (b := m))
    (adexp : ADEXP Point a b)
    (split : List Point × List Point) :
    Option
      (FrontSplit
        (reconciliationSystem Point Route)
        icaoLeg
        adexp) :=
  match hrest :
      adexpOfRaw? (Point := Point) (a := m) (b := b) split.2 with
  | none =>
      none
  | some rest =>
      if htail : split.1 ++ split.2 = adexpTail adexp then
        some
          { adexpPrefix :=
              adexpLegPath (Point := Point) (a := a) (b := m) split.1
            rest := rest
            factor := by
              apply adexp_eq_of_tail_eq
              have hrest_raw := adexpOfRaw?_sound (Point := Point) hrest
              have hraw_tail :
                  split.2 = m :: adexpTail rest := by
                simpa [adexpWaypoints] using hrest_raw.symm
              rw [← htail, hraw_tail]
              symm
              change
                adexpTail
                    (Plan.append
                      (adexpLegPath
                        (Point := Point) (a := a) (b := m) split.1)
                      rest) =
                  split.1 ++ m :: adexpTail rest
              simp [adexpTail_append, adexpTail_adexpLegPath,
                List.append_assoc]
            nontrivial := adexpLegPath_length_pos (Point := Point) split.1
            legReconciliation :=
              legReconciliation icaoLeg split.1 }
      else
        none

theorem frontSplitOfRawSplit?_some [DecidableEq Point]
    {a m b : Point}
    {icaoLeg :
      LegPlan (reconciliationSystem Point Route).ICAO (a := a) (b := m)}
    {adexp : ADEXP Point a b}
    {split : List Point × List Point}
    {front :
      FrontSplit
        (reconciliationSystem Point Route)
        icaoLeg
        adexp}
    (h :
      frontSplitOfRawSplit? icaoLeg adexp split =
          some front) :
    front.adexpPrefix =
        adexpLegPath (Point := Point) (a := a) (b := m) split.1 ∧
      adexpWaypoints front.rest = split.2 := by
  unfold frontSplitOfRawSplit? at h
  split at h
  · contradiction
  next rest hparse =>
    split at h
    · cases h
      constructor
      · rfl
      · exact adexpOfRaw?_sound (Point := Point) hparse
    · contradiction

def frontSplits [DecidableEq Point] :
    FrontSplitEnumerator (reconciliationSystem Point Route) where
  enumerate := by
    intro a m b icaoLeg adexp
    change Point at a
    change Point at m
    change Point at b
    refine
      { splits :=
          (Raw.splits (Point := Point) (adexpTail adexp)).filterMap
            (frontSplitOfRawSplit? icaoLeg adexp)
        sound := ?_
        complete := ?_
        nodup := ?_ }
    · intro s _hmem
      exact ⟨s.factor, s.nontrivial, ⟨s.legReconciliation⟩⟩
    · intro adexpPrefix rest hfactor _hnontrivial hleg
      rcases hleg with ⟨legRec⟩
      rcases legRec with ⟨combinedLeg, legToIcao, legToAdexp, legUnique⟩
      rcases combinedLeg with ⟨combinedLegVal, hcombinedLeg⟩
      rcases hcombinedLeg with ⟨via, hcombinedLeg⟩
      cases hcombinedLeg
      let rawSplit : List Point × List Point :=
        (via.through, adexpWaypoints rest)
      have hprefix :
          adexpLegPath (Point := Point) (a := a) (b := m) via.through =
            adexpPrefix := by
        have h := legToAdexp
        change projectADEXP (Plan.single via) = adexpPrefix at h
        simpa [projectADEXP, Plan.single, Plan.append] using h
      have htail :
          rawSplit.1 ++ rawSplit.2 = adexpTail adexp := by
        have htail_factor := congrArg adexpTail hfactor
        rw [hprefix.symm] at htail_factor
        rw [htail_factor]
        symm
        change
          adexpTail
              (Plan.append
                (adexpLegPath (Point := Point) (a := a) (b := m) via.through)
                rest) =
            (via.through : List (reconciliationSystem Point Route).Point) ++
              (m :: adexpTail rest)
        simp [adexpTail_append, adexpTail_adexpLegPath, List.append_assoc]
      refine
        ⟨
          { adexpPrefix :=
              adexpLegPath (Point := Point) (a := a) (b := m) rawSplit.1
            rest := rest
            factor := by
              rw [hprefix]
              exact hfactor
            nontrivial := adexpLegPath_length_pos (Point := Point) rawSplit.1
            legReconciliation :=
              legReconciliation icaoLeg rawSplit.1 },
          ?_, ?_, ?_⟩
      · apply List.mem_filterMap.2
        refine ⟨rawSplit, ?_, ?_⟩
        · exact (Raw.mem_splits_iff (Point := Point) rawSplit).2 htail
        · have hcomplete :
              adexpOfRaw? (Point := Point) (a := m) (b := b) rawSplit.2 =
                some rest := by
            simpa [rawSplit] using adexpOfRaw?_complete (Point := Point) rest
          unfold frontSplitOfRawSplit?
          split
          · rename_i hnone
            rw [hcomplete] at hnone
            contradiction
          next parsed hparse =>
            have hparsed : parsed = rest := by
              rw [hcomplete] at hparse
              cases hparse
              rfl
            cases hparsed
            split
            · rfl
            · rename_i hnot
              exact False.elim (hnot htail)
      · exact hprefix
      · rfl
    · apply List.Nodup.filterMap
      · intro left right front hleft hright
        rcases frontSplitOfRawSplit?_some hleft with
          ⟨hleft_prefix, hleft_rest⟩
        rcases frontSplitOfRawSplit?_some hright with
          ⟨hright_prefix, hright_rest⟩
        apply Prod.ext
        · exact adexpLegPath_inj (Point := Point)
            (hleft_prefix.symm.trans hright_prefix)
        · exact hleft_rest.symm.trans hright_rest
      · exact Raw.splits_nodup (Point := Point)

def laws :
    FrontRecursiveReconciliationLaws
      (reconciliationSystem Point Route) where
  toIcao_reflects_identity := by
    intro a b combined hicao_id
    apply
      (Plan.isIdPlan_iff_length_zero
        (Route := Via Point Route) combined).2
    have hlen :
        Plan.length (projectICAO combined) = 0 :=
      (Plan.isIdPlan_iff_length_zero
        (Route := Route) (projectICAO combined)).1 hicao_id
    simpa [projectICAO_length combined] using hlen
  front_complete := by
    intro a m b icaoLeg icaoRest adexp combined htoIcao htoAdexp
    rcases icaoLeg with ⟨icaoLegVal, hicaoLeg⟩
    cases icaoLegVal with
    | nil point =>
        have hpos := Plan.leg_length_positive hicaoLeg
        simp [Plan.length] at hpos
    | cons route icaoTail =>
        cases icaoTail with
        | nil _ =>
            cases combined with
            | nil point =>
                change
                  projectICAO (Plan.nil (Route := Via Point Route) a) =
                    Plan.append (Plan.single route) icaoRest at htoIcao
                have hlen := congrArg Plan.length htoIcao
                simp [projectICAO, mapRoutes, Plan.single, Plan.append,
                  Plan.length] at hlen
            | cons via combinedRest =>
                change
                  Plan.cons via.route (projectICAO combinedRest) =
                    Plan.cons route icaoRest at htoIcao
                cases htoIcao
                refine
                  { adexpPrefix :=
                      adexpLegPath (Point := Point) (a := a) (b := m) via.through
                    adexpRest :=
                      projectADEXP combinedRest
                    adexp_factor := ?_
                    adexpPrefix_nontrivial :=
                      adexpLegPath_length_pos (Point := Point) via.through
                    legReconciliation :=
                      legReconciliation ⟨Plan.single via.route, ⟨via.route, rfl⟩⟩
                        via.through
                    combinedRest := combinedRest
                    combined_factor := rfl
                    rest_toIcao_eq := rfl
                    rest_toAdexp_eq := rfl }
                rw [← htoAdexp]
                rfl
        | cons next tail =>
            have hlen := Plan.leg_length_eq_one hicaoLeg
            simp [Plan.length] at hlen
  front_comp_no_confusion := by
    intro a m b leftLeg rightLeg leftRest rightRest hsame
    rcases leftLeg with ⟨leftLegVal, hleftLeg⟩
    rcases rightLeg with ⟨rightLegVal, hrightLeg⟩
    rcases hleftLeg with ⟨leftVia, hleftLeg⟩
    rcases hrightLeg with ⟨rightVia, hrightLeg⟩
    cases hleftLeg
    cases hrightLeg
    simp [Plan.single] at hsame
    cases hsame
    exact ⟨rfl, rfl⟩

end ReconciliationSearch

/-- The single concrete reconciler, exposed for raw waypoint-stream callers. -/
def rawReconciliationCandidates {Point : Type u}
    {Route : Point -> Point -> Type u}
    [DecidableEq Point]
    {a b : Point}
    (icao : ICAO Point Route a b)
    (rawAdexp : List Point) :
    List (Combined Point Route a b) :=
  match adexpOfRaw? (Point := Point) (a := a) (b := b) rawAdexp with
  | none =>
      []
  | some adexp =>
      Reconciliation.FrontRecursive.candidates
        (ReconciliationSearch.identityBaseEnumerator)
        (ReconciliationSearch.frontSplits)
        (ReconciliationSearch.laws)
        icao adexp

theorem rawReconciliationCandidates_sound {Point : Type u}
    {Route : Point -> Point -> Type u}
    [DecidableEq Point]
    {a b : Point}
    (icao : ICAO Point Route a b)
    (rawAdexp : List Point)
    (combined : Combined Point Route a b)
    (hmem :
      combined ∈ rawReconciliationCandidates icao rawAdexp) :
    projectICAO combined = icao ∧
      Raw.flattenCombined combined = rawAdexp := by
  unfold rawReconciliationCandidates at hmem
  cases hraw :
      adexpOfRaw? (Point := Point) (a := a) (b := b) rawAdexp with
  | none =>
      simp [hraw] at hmem
  | some adexp =>
      simp [hraw] at hmem
      rcases
        (Reconciliation.FrontRecursive.enumeration
          (ReconciliationSearch.identityBaseEnumerator)
          (ReconciliationSearch.frontSplits)
          (ReconciliationSearch.laws)
          icao adexp).sound
          combined hmem with
        ⟨hicao, hadexp⟩
      constructor
      · exact hicao
      · have hraw_adexp := adexpOfRaw?_sound (Point := Point) hraw
        change combinedWaypoints combined = rawAdexp
        rw [← adexpWaypoints_projectADEXP combined]
        change projectADEXP combined = adexp at hadexp
        rw [hadexp, hraw_adexp]

theorem rawReconciliationCandidates_complete {Point : Type u}
    {Route : Point -> Point -> Type u}
    [DecidableEq Point]
    {a b : Point}
    (icao : ICAO Point Route a b)
    (rawAdexp : List Point)
    (combined : Combined Point Route a b)
    (hicao : projectICAO combined = icao)
    (hraw : Raw.flattenCombined combined = rawAdexp) :
    combined ∈ rawReconciliationCandidates icao rawAdexp := by
  unfold rawReconciliationCandidates
  have hconv :
      adexpOfRaw? (Point := Point) (a := a) (b := b) rawAdexp =
        some (projectADEXP combined) := by
    rw [← hraw]
    change
      adexpOfRaw? (Point := Point) (a := a) (b := b)
        (combinedWaypoints combined) = some (projectADEXP combined)
    rw [← adexpWaypoints_projectADEXP combined]
    exact adexpOfRaw?_complete (Point := Point) (projectADEXP combined)
  simp [hconv]
  exact
    (Reconciliation.FrontRecursive.enumeration
      (ReconciliationSearch.identityBaseEnumerator)
      (ReconciliationSearch.frontSplits)
      (ReconciliationSearch.laws)
      icao (projectADEXP combined)).complete
      combined hicao rfl

theorem rawReconciliationCandidates_nodup {Point : Type u}
    {Route : Point -> Point -> Type u}
    [DecidableEq Point]
    {a b : Point}
    (icao : ICAO Point Route a b)
    (rawAdexp : List Point) :
    (rawReconciliationCandidates icao rawAdexp).Nodup := by
  unfold rawReconciliationCandidates
  cases adexpOfRaw? (Point := Point) (a := a) (b := b) rawAdexp with
  | none =>
      simp
  | some adexp =>
      exact
        (Reconciliation.FrontRecursive.enumeration
          (ReconciliationSearch.identityBaseEnumerator)
          (ReconciliationSearch.frontSplits)
          (ReconciliationSearch.laws)
          icao adexp).nodup

def RawIcaoSuccess {Point : Type u}
    {Route : Point -> Point -> Type u}
    (uk : Point -> Prop)
    {a b : Point}
    (icao : ICAO Point Route a b)
    (rawAdexp : List Point)
    (start finish : Point)
    (portion : ICAO Point Route start finish) : Prop :=
  ∃ combined : Combined Point Route a b,
    projectICAO combined = icao ∧
    Raw.flattenCombined combined = rawAdexp ∧
    ∃ selected :
      Subplan (reconciliationSystem Point Route).Combined.cat combined,
      HasUK
        (UK.rawRelevance uk)
        combined ∧
      LeastUKSubplan
        (UK.rawRelevance uk)
        selected ∧
      ∃ hstart : selected.i = start,
      ∃ hfinish : selected.j = finish,
        hstart ▸ hfinish ▸ projectICAO selected.middle = portion

def RawIcaoResultCorrect {Point : Type u}
    {Route : Point -> Point -> Type u}
    [DecidableEq Point]
    (uk : Point -> Prop)
    {a b : Point}
    (icao : ICAO Point Route a b)
    (rawAdexp : List Point) :
    RawIcaoResult Point Route -> Prop
  | RawIcaoResult.ok start finish portion =>
      RawIcaoSuccess uk icao rawAdexp start finish portion
  | RawIcaoResult.error RawErr.nonUkPlan =>
      ¬ ∃ start finish,
        ∃ portion : ICAO Point Route start finish,
          RawIcaoSuccess uk icao rawAdexp start finish portion
  | RawIcaoResult.error RawErr.cannotReconcileIcaoAdexp =>
      ∀ combined : Combined Point Route a b,
        ¬ (projectICAO combined = icao ∧
          Raw.flattenCombined combined = rawAdexp)
  | RawIcaoResult.error RawErr.ambiguousReconciliationsOfIcaoAdexp =>
      ∃ left right : Combined Point Route a b,
        left ≠ right ∧
        projectICAO left = icao ∧
        Raw.flattenCombined left = rawAdexp ∧
        projectICAO right = icao ∧
        Raw.flattenCombined right = rawAdexp

/--
Concrete top-level operation matching the shape of `app/Main.hs`:

* reconcile an ICAO plan against a raw ADEXP waypoint stream;
* fail when reconciliation has zero or multiple candidates;
* for the unique candidate, compute the concrete UK portion;
* project the selected combined middle back to ICAO.
-/
def ukPortionOfIcaoRaw {Point : Type u}
    {Route : Point -> Point -> Type u}
    [DecidableEq Point]
    (uk : Point -> Prop)
    [DecidablePred uk]
    {a b : Point}
    (icao : ICAO Point Route a b)
    (rawAdexp : List Point) :
    RawIcaoResult Point Route :=
  match rawReconciliationCandidates icao rawAdexp with
  | [] =>
      RawIcaoResult.error RawErr.cannotReconcileIcaoAdexp
  | [combined] =>
      match decideByConcreteChopping uk combined with
      | LeastUKSubplanDecision.none _noUK =>
          RawIcaoResult.error RawErr.nonUkPlan
      | LeastUKSubplanDecision.found selected _hasUK _least =>
          RawIcaoResult.ok
            selected.i
            selected.j
            (projectICAO selected.middle)
  | _ :: _ :: _ =>
      RawIcaoResult.error RawErr.ambiguousReconciliationsOfIcaoAdexp

theorem ukPortionOfIcaoRaw_correct
    {Point : Type u}
    {Route : Point -> Point -> Type u}
    [DecidableEq Point]
    (uk : Point -> Prop)
    [DecidablePred uk]
    {a b : Point}
    (icao : ICAO Point Route a b)
    (rawAdexp : List Point) :
    RawIcaoResultCorrect uk icao rawAdexp
      (ukPortionOfIcaoRaw uk icao rawAdexp) := by
  unfold ukPortionOfIcaoRaw
  cases hcandidates :
      rawReconciliationCandidates icao rawAdexp with
  | nil =>
      intro combined hmatch
      have hmem :
          combined ∈
            rawReconciliationCandidates icao rawAdexp :=
        rawReconciliationCandidates_complete icao rawAdexp combined hmatch.1 hmatch.2
      rw [hcandidates] at hmem
      cases hmem
  | cons combined restCandidates =>
      cases restCandidates with
      | nil =>
          cases hextract :
              decideByConcreteChopping uk combined with
          | none noUK =>
              simp [hextract]
              intro success
              rcases success with
                ⟨start, finish, portion, reco, hreco_icao, hreco_raw,
                  selected, hasUK, least, _hstart, _hfinish, _hportion⟩
              have hmem :
                  reco ∈
                    rawReconciliationCandidates icao rawAdexp :=
                rawReconciliationCandidates_complete icao rawAdexp reco hreco_icao hreco_raw
              rw [hcandidates] at hmem
              have hreco_eq : reco = combined := by
                simpa using hmem
              rw [hreco_eq] at hasUK
              exact noUK hasUK
          | found selected hasUK least =>
              simp [hextract]
              rcases
                rawReconciliationCandidates_sound icao rawAdexp combined
                  (by rw [hcandidates]; simp) with
                ⟨hicao, hraw⟩
              exact
                ⟨combined, hicao, hraw, selected, hasUK, least,
                  rfl, rfl, rfl⟩
      | cons second rest =>
          simp
          have hnodup :
              (combined :: second :: rest).Nodup := by
            simpa [hcandidates] using
              rawReconciliationCandidates_nodup icao rawAdexp
          have hne : combined ≠ second := by
            intro heq
            rw [heq] at hnodup
            cases hnodup with
            | cons hnotin _ =>
                exact hnotin second (by simp) rfl
          rcases
            rawReconciliationCandidates_sound icao rawAdexp combined
              (by rw [hcandidates]; simp) with
            ⟨hcombined_icao, hcombined_raw⟩
          rcases
            rawReconciliationCandidates_sound icao rawAdexp second
              (by rw [hcandidates]; simp) with
            ⟨hsecond_icao, hsecond_raw⟩
          exact
            ⟨combined, second, hne, hcombined_icao, hcombined_raw,
              hsecond_icao, hsecond_raw⟩

end Implementation

namespace Examples

open Extraction

abbrev ExampleRoute (_ _ : Nat) : Type :=
  Unit

def exampleUK (point : Nat) : Prop :=
  point = 20 ∨ point = 40

instance exampleUKDecidable : DecidablePred exampleUK :=
  fun point => inferInstanceAs (Decidable (point = 20 ∨ point = 40))

def examplePlan : Combined Nat ExampleRoute 0 5 :=
  Plan.cons ({ route := (), through := [] } :
      Via Nat ExampleRoute 0 1)
    (Plan.cons ({ route := (), through := [20] } :
        Via Nat ExampleRoute 1 2)
      (Plan.cons ({ route := (), through := [] } :
          Via Nat ExampleRoute 2 3)
        (Plan.cons ({ route := (), through := [40] } :
            Via Nat ExampleRoute 3 4)
          (Plan.cons ({ route := (), through := [] } :
              Via Nat ExampleRoute 4 5)
            (Plan.nil 5)))))

def noUKPlan : Combined Nat ExampleRoute 0 3 :=
  Plan.cons ({ route := (), through := [] } :
      Via Nat ExampleRoute 0 1)
    (Plan.cons ({ route := (), through := [] } :
        Via Nat ExampleRoute 1 2)
      (Plan.cons ({ route := (), through := [] } :
          Via Nat ExampleRoute 2 3)
        (Plan.nil 3)))

def singleUKPlan : Combined Nat ExampleRoute 0 4 :=
  Plan.cons ({ route := (), through := [] } :
      Via Nat ExampleRoute 0 1)
    (Plan.cons ({ route := (), through := [20] } :
        Via Nat ExampleRoute 1 2)
      (Plan.cons ({ route := (), through := [] } :
          Via Nat ExampleRoute 2 3)
        (Plan.cons ({ route := (), through := [] } :
            Via Nat ExampleRoute 3 4)
          (Plan.nil 4))))

def selectedSummary
    {a b : Nat}
    {whole : Combined Nat ExampleRoute a b}
    (decision :
      LeastUKSubplanDecision
        (reconciliationSystem Nat ExampleRoute).Combined
        (UK.rawRelevance (Point := Nat) (Route := ExampleRoute) exampleUK)
        whole) :
    Option (Nat × Nat × Nat) :=
  match decision with
  | LeastUKSubplanDecision.none _ =>
      none
  | LeastUKSubplanDecision.found selected _ _ =>
      some (selected.i, selected.j, Plan.length selected.middle)

/-
The concrete chopper removes the outside first and last legs, keeping the
smallest middle from endpoint 1 to endpoint 4. The summary is
`(start endpoint, finish endpoint, ICAO leg count)`.
-/
#eval selectedSummary
  (decideByConcreteChopping (Point := Nat) (Route := ExampleRoute) exampleUK examplePlan)

/- With no UK-relevant point or through-point, extraction returns no result. -/
#eval selectedSummary
  (decideByConcreteChopping (Point := Nat) (Route := ExampleRoute) exampleUK noUKPlan)

/- A single UK through-point gives the one concrete leg that contains it. -/
#eval selectedSummary
  (decideByConcreteChopping (Point := Nat) (Route := ExampleRoute) exampleUK singleUKPlan)

abbrev RawRoute (_ _ : Nat) : Type :=
  Nat

/- Nat-coded copy of the `app/Main.hs` example:
   F=100, Q=101, T=102, O=103, P=104, Y=105, U=106,
   S=107, C=108, A=109, E=110, X=111, W=112, B=113. -/
def appUKRaw (point : Nat) : Prop :=
  point = 102 ∨ point = 109 ∨ point = 103 ∨ point = 110 ∨
    point = 111 ∨ point = 104 ∨ point = 112

instance appUKRawDecidable : DecidablePred appUKRaw :=
  fun point => inferInstanceAs
    (Decidable
      (point = 102 ∨ point = 109 ∨ point = 103 ∨ point = 110 ∨
        point = 111 ∨ point = 104 ∨ point = 112))

def appIcaoRaw : ICAO Nat RawRoute 100 106 :=
  Plan.cons (start := 100) (mid := 101) (finish := 106)
    (4 : RawRoute 100 101)
    (Plan.cons (start := 101) (mid := 102) (finish := 106)
      (2 : RawRoute 101 102)
      (Plan.cons (start := 102) (mid := 103) (finish := 106)
        (8 : RawRoute 102 103)
        (Plan.cons (start := 103) (mid := 104) (finish := 106)
          (5 : RawRoute 103 104)
          (Plan.cons (start := 104) (mid := 105) (finish := 106)
            (1 : RawRoute 104 105)
            (Plan.cons (start := 105) (mid := 106) (finish := 106)
              (9 : RawRoute 105 106)
              (Plan.nil 106))))))

def appRawAdexp : List Nat :=
  [100, 107, 101, 108, 102, 109, 103, 110, 111, 104, 112, 113, 101, 105, 106]

def smallRawIcao : ICAO Nat RawRoute 100 106 :=
  Plan.cons (start := 100) (mid := 101) (finish := 106)
    (1 : RawRoute 100 101)
    (Plan.cons (start := 101) (mid := 106) (finish := 106)
      (2 : RawRoute 101 106)
      (Plan.nil 106))

def ambiguousIcaoRaw : ICAO Nat RawRoute 200 202 :=
  Plan.cons (start := 200) (mid := 201) (finish := 202)
    (10 : RawRoute 200 201)
    (Plan.cons (start := 201) (mid := 202) (finish := 202)
      (20 : RawRoute 201 202)
      (Plan.nil 202))

def ambiguousUKRaw (point : Nat) : Prop :=
  point = 201

instance ambiguousUKRawDecidable : DecidablePred ambiguousUKRaw :=
  fun point => inferInstanceAs (Decidable (point = 201))

def throughOnlyUKRaw (point : Nat) : Prop :=
  point = 107

instance throughOnlyUKRawDecidable : DecidablePred throughOnlyUKRaw :=
  fun point => inferInstanceAs (Decidable (point = 107))

def icaoTokens :
    {a b : Nat} ->
      ICAO Nat RawRoute a b ->
        List Nat
  | _, _, Plan.nil point => [point]
  | start, _, Plan.cons route rest =>
      start :: route :: icaoTokens rest

def rawResultTokens
    (result : RawIcaoResult Nat RawRoute) :
    Except RawErr (List Nat) :=
  match result with
  | RawIcaoResult.error err =>
      Except.error err
  | RawIcaoResult.ok _ _ plan =>
      Except.ok (icaoTokens plan)

/- Raw ADEXP end-to-end success on the `app/Main.hs` example. -/
#eval rawResultTokens
  (ukPortionOfIcaoRaw (Point := Nat) (Route := RawRoute)
    appUKRaw appIcaoRaw appRawAdexp)

/- Raw ADEXP end-to-end no-UK case. -/
#eval rawResultTokens
  (ukPortionOfIcaoRaw (Point := Nat) (Route := RawRoute)
    appUKRaw smallRawIcao [100, 107, 101, 106])

/- Raw ADEXP impossible reconciliation: the required ICAO waypoint is absent. -/
#eval rawResultTokens
  (ukPortionOfIcaoRaw (Point := Nat) (Route := RawRoute)
    appUKRaw smallRawIcao [100, 107, 106])

/- Raw ADEXP split ambiguity caused by a repeated waypoint. -/
#eval rawResultTokens
  (ukPortionOfIcaoRaw (Point := Nat) (Route := RawRoute)
    ambiguousUKRaw ambiguousIcaoRaw [200, 201, 201, 202])

/- Raw ADEXP success where UK relevance comes only from an ADEXP through-point. -/
#eval rawResultTokens
  (ukPortionOfIcaoRaw (Point := Nat) (Route := RawRoute)
    throughOnlyUKRaw smallRawIcao [100, 107, 101, 106])

end Examples

end Model.Concrete

end Formal.Categorical
