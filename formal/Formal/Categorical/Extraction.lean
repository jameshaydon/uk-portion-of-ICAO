import Formal.Categorical.Core.Proofs

namespace Formal.Categorical

universe u v w

namespace Extraction

/-- A point occurrence is a subplan whose middle is an identity plan. -/
def IsPointSubplan
    {Point : Type u}
    (S : FreeFinitePlanSystem.{u, v} Point)
    {a b : Point}
    {whole : S.Plan a b}
    (s : Subplan S.cat whole) : Prop :=
  IsIdPlan s.middle

/-- A leg occurrence is a subplan whose middle is one generating leg. -/
def IsLegSubplan
    {Point : Type u}
    (S : FreeFinitePlanSystem.{u, v} Point)
    {a b : Point}
    {whole : S.Plan a b}
    (s : Subplan S.cat whole) : Prop :=
  S.IsLeg s.middle

abbrev PointOccurrence
    {Point : Type u}
    (S : FreeFinitePlanSystem.{u, v} Point)
    {a b : Point}
    (whole : S.Plan a b) : Type (max u v) :=
  { s : Subplan S.cat whole // IsPointSubplan S s }

abbrev LegOccurrence
    {Point : Type u}
    (S : FreeFinitePlanSystem.{u, v} Point)
    {a b : Point}
    (whole : S.Plan a b) : Type (max u v) :=
  { s : Subplan S.cat whole // IsLegSubplan S s }

/--
An atomic occurrence is still just one subplan occurrence, with a proof that
its middle is one of the shapes extraction cares about.
-/
structure AtomicOccurrence
    {Point : Type u}
    (S : FreeFinitePlanSystem.{u, v} Point)
    {a b : Point}
    (whole : S.Plan a b) where
  subplan : Subplan S.cat whole
  isAtomic : IsPointSubplan S subplan ∨ IsLegSubplan S subplan

/-- UK relevance for an atomic occurrence, parameterized by the two base predicates. -/
def AtomicOccurrenceHasUK
    {Point : Type u}
    (S : FreeFinitePlanSystem.{u, v} Point)
    (pointHasUK :
      {a b : Point} -> {whole : S.Plan a b} ->
        PointOccurrence S whole -> Prop)
    (legHasUK :
      {a b : Point} -> {whole : S.Plan a b} ->
        LegOccurrence S whole -> Prop)
    {a b : Point}
    {whole : S.Plan a b}
    (atom : AtomicOccurrence S whole) : Prop :=
  (∃ h : IsPointSubplan S atom.subplan,
    pointHasUK ⟨atom.subplan, h⟩) ∨
  (∃ h : IsLegSubplan S atom.subplan,
    legHasUK ⟨atom.subplan, h⟩)

/--
Domain-specific UK relevance predicates.

Boundary policy belongs in these predicates. For a combined ICAO/ADEXP model,
`legHasUK` can encode closed, half-open, or executable conventions.
-/
structure UKRelevance
    {Point : Type u}
    (S : FreeFinitePlanSystem.{u, v} Point) where
  pointHasUK :
    {a b : Point} -> {whole : S.Plan a b} ->
      PointOccurrence S whole -> Prop
  legHasUK :
    {a b : Point} -> {whole : S.Plan a b} ->
      LegOccurrence S whole -> Prop
  /--
  If a UK atom of the whole plan lies inside a leg occurrence, then that leg
  occurrence is UK.
  -/
  legHasUK_of_atom_le :
    {a b : Point} -> {whole : S.Plan a b} ->
      (atom : AtomicOccurrence S whole) ->
      (leg : LegOccurrence S whole) ->
      AtomicOccurrenceHasUK S pointHasUK legHasUK atom ->
      atom.subplan <= leg.val ->
        legHasUK leg

def AtomHasUK
    {Point : Type u}
    {S : FreeFinitePlanSystem.{u, v} Point}
    (rel : UKRelevance S)
    {a b : Point}
    {whole : S.Plan a b}
    (atom : AtomicOccurrence S whole) : Prop :=
  AtomicOccurrenceHasUK S rel.pointHasUK rel.legHasUK atom

/-- Every UK-relevant atomic occurrence of `whole` lies inside `s`. -/
def CoversUK
    {Point : Type u}
    {S : FreeFinitePlanSystem.{u, v} Point}
    (rel : UKRelevance S)
    {a b : Point}
    {whole : S.Plan a b}
    (s : Subplan S.cat whole) : Prop :=
  ∀ atom : AtomicOccurrence S whole,
    AtomHasUK rel atom -> atom.subplan <= s

/-- The whole plan contains at least one UK-relevant atomic occurrence. -/
def HasUK
    {Point : Type u}
    {S : FreeFinitePlanSystem.{u, v} Point}
    (rel : UKRelevance S)
    {a b : Point}
    (whole : S.Plan a b) : Prop :=
  ∃ atom : AtomicOccurrence S whole, AtomHasUK rel atom

/-- The smallest occurrence interval covering all UK-relevant atoms. -/
def LeastUKSubplan
    {Point : Type u}
    {S : FreeFinitePlanSystem.{u, v} Point}
    (rel : UKRelevance S)
    {a b : Point}
    {whole : S.Plan a b}
    (s : Subplan S.cat whole) : Prop :=
  IsLeast { t : Subplan S.cat whole | CoversUK rel t } s

theorem whole_covers
    {Point : Type u}
    {S : FreeFinitePlanSystem.{u, v} Point}
    (rel : UKRelevance S)
    {a b : Point}
    {whole : S.Plan a b} :
    CoversUK rel (Subplan.whole S.cat whole) := by
  grind [CoversUK, Subplan.le_whole]

/-- Result of deciding the least UK subplan for one combined plan. -/
inductive LeastUKSubplanDecision
    {Point : Type u}
    (S : FreeFinitePlanSystem.{u, v} Point)
    (rel : UKRelevance S)
    {a b : Point}
    (whole : S.Plan a b) : Type (max u v) where
  | none :
      (noUK : ¬ HasUK rel whole) ->
        LeastUKSubplanDecision S rel whole
  | found :
      (selected : Subplan S.cat whole) ->
      (hasUK : HasUK rel whole) ->
      LeastUKSubplan rel selected ->
        LeastUKSubplanDecision S rel whole

end Extraction

end Formal.Categorical
