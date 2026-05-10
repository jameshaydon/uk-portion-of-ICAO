import Mathlib.Data.List.Basic
import Mathlib.Order.Basic
import Mathlib.Order.Bounds.Defs

/-!
# An index-based extraction spec

This is the lower-level alternative described in `blog-draft.md`: instead of
saying that a subplan is a factorisation

```
whole = pre ≫ middle ≫ post
```

we say that the selected ICAO portion is an inclusive pair of raw ICAO token
indexes.

The point of this file is not to replace the categorical spec.  It is a small
piece of Lean showing what the integer-index version has to talk about.
-/

namespace Formal.IndexedSpec

universe u v

/--
A raw ICAO token stream is the imperative representation:

```
waypoint, route, waypoint, route, ..., waypoint
```

This file does not try to prove that the list alternates correctly.  The
imperative code checks that separately with `len(icao) != 0` and odd length.
-/
inductive IcaoToken (Waypoint : Type u) (Route : Type v) where
  | waypoint : Waypoint -> IcaoToken Waypoint Route
  | route : Route -> IcaoToken Waypoint Route

abbrev RawIcaoPlan (Waypoint : Type u) (Route : Type v) :=
  List (IcaoToken Waypoint Route)

/-- A raw ADEXP stream is just the expanded waypoint stream. -/
abbrev RawAdexpPlan (Waypoint : Type u) :=
  List Waypoint

/--
The result shape returned by the imperative implementation: inclusive raw ICAO
token indexes.

The endpoint parity proofs say that both indexes are waypoint-token positions,
not route-name positions.  For example, selecting waypoint numbers `l` and `r`
returns raw token indexes `2*l` and `2*r`.
-/
structure RawIcaoInterval
    {Waypoint : Type u}
    {Route : Type v}
    (icao : RawIcaoPlan Waypoint Route) where
  start : Nat
  finish : Nat
  start_valid : start < icao.length
  finish_valid : finish < icao.length
  ordered : start <= finish
  start_is_waypoint_index : start % 2 = 0
  finish_is_waypoint_index : finish % 2 = 0

namespace RawIcaoInterval

variable {Waypoint : Type u}
variable {Route : Type v}
variable {icao : RawIcaoPlan Waypoint Route}

/--
Interval containment, expressed entirely as arithmetic on raw token indexes.

This is the index-indirection that the categorical version avoids.
-/
instance instPreorder : Preorder (RawIcaoInterval icao) where
  le small large :=
    large.start <= small.start ∧ small.finish <= large.finish
  le_refl s :=
    ⟨Nat.le_refl s.start, Nat.le_refl s.finish⟩
  le_trans a b c hab hbc :=
    ⟨Nat.le_trans hbc.1 hab.1, Nat.le_trans hab.2 hbc.2⟩

/-- The actual raw ICAO token slice denoted by an inclusive interval. -/
def tokens (s : RawIcaoInterval icao) : RawIcaoPlan Waypoint Route :=
  (icao.drop s.start).take (s.finish - s.start + 1)

end RawIcaoInterval

/--
Internal trimming often works with waypoint numbers rather than raw token
indexes.  This mirrors `uk_window_from_reconciliation` in `fast.py`.
-/
structure WaypointWindow
    {Waypoint : Type u}
    {Route : Type v}
    (icao : RawIcaoPlan Waypoint Route) where
  start_waypoint : Nat
  finish_waypoint : Nat
  start_token_valid : 2 * start_waypoint < icao.length
  finish_token_valid : 2 * finish_waypoint < icao.length
  ordered : start_waypoint <= finish_waypoint

/-- Convert waypoint numbers back to the raw ICAO token indexes returned. -/
def WaypointWindow.toRawIcaoInterval
    {Waypoint : Type u}
    {Route : Type v}
    {icao : RawIcaoPlan Waypoint Route}
    (w : WaypointWindow icao) : RawIcaoInterval icao where
  start := 2 * w.start_waypoint
  finish := 2 * w.finish_waypoint
  start_valid := w.start_token_valid
  finish_valid := w.finish_token_valid
  ordered := Nat.mul_le_mul_left 2 w.ordered
  start_is_waypoint_index := Nat.mul_mod_right 2 w.start_waypoint
  finish_is_waypoint_index := Nat.mul_mod_right 2 w.finish_waypoint

/--
The atomic occurrence shapes that the extraction spec cares about.

An `icaoWaypoint` occurrence is a waypoint that appeared explicitly in the raw
ICAO stream.  An `adexpWaypoint` occurrence is an expanded waypoint that came
from the ADEXP stream, possibly inside an ICAO leg.  An `icaoLeg` occurrence is
the raw ICAO leg between two adjacent waypoint tokens.
-/
inductive OccurrenceKind where
  | icaoWaypoint
  | adexpWaypoint
  | icaoLeg
  deriving DecidableEq, Repr

/--
One occurrence in the reconciled, combined view of the flight plan.

The occurrence itself is not identified by waypoint text alone.  It carries the
raw ICAO interval that covers this occurrence.  This is what stops repeated
waypoint names from being collapsed together.
-/
structure CombinedOccurrence
    {Waypoint : Type u}
    {Route : Type v}
    (icao : RawIcaoPlan Waypoint Route) where
  kind : OccurrenceKind
  spanInIcao : RawIcaoInterval icao

/--
The index-based stand-in for a combined plan.

In the categorical files, the combined plan is an endpoint-indexed path whose
legs carry ICAO route labels and ADEXP through-points.  In this indexed sketch,
we flatten the reconciled result into a list of located occurrences.  Each
occurrence says which raw ICAO interval covers it.
-/
abbrev IndexedCombinedPlan
    {Waypoint : Type u}
    {Route : Type v}
    (icao : RawIcaoPlan Waypoint Route) :=
  List (CombinedOccurrence icao)

/--
The data available after reconciliation, in the low-level indexed view.

`icao` is the raw alternating token stream.
`adexp` is the expanded waypoint stream.
`combined` is the reconciled list of located occurrences used by the spec.
-/
structure IndexedReconciledPlan
    (Waypoint : Type u)
    (Route : Type v) where
  icao : RawIcaoPlan Waypoint Route
  adexp : RawAdexpPlan Waypoint
  combined : IndexedCombinedPlan icao

/--
Domain-specific UK relevance for occurrences of the combined plan.

For a concrete model this would inspect the occurrence's waypoint or leg data.
Here it is abstract, matching `UKRelevance` in the categorical spec.
-/
structure UKRelevance
    {Waypoint : Type u}
    {Route : Type v}
    (p : IndexedReconciledPlan Waypoint Route) where
  occurrenceHasUK :
    ∀ occurrence : CombinedOccurrence p.icao,
      occurrence ∈ p.combined -> Prop

/--
The selected raw ICAO interval covers the UK part when every UK-relevant
combined occurrence has its covering ICAO interval inside the selected interval.
-/
def CoversUK
    {Waypoint : Type u}
    {Route : Type v}
    {p : IndexedReconciledPlan Waypoint Route}
    (rel : UKRelevance p)
    (selected : RawIcaoInterval p.icao) : Prop :=
  ∀ occurrence : CombinedOccurrence p.icao,
    ∀ hmem : occurrence ∈ p.combined,
      rel.occurrenceHasUK occurrence hmem ->
        occurrence.spanInIcao <= selected

/-- The reconciled plan contains at least one UK-relevant occurrence. -/
def HasUK
    {Waypoint : Type u}
    {Route : Type v}
    {p : IndexedReconciledPlan Waypoint Route}
    (rel : UKRelevance p) : Prop :=
  ∃ occurrence : CombinedOccurrence p.icao,
    ∃ hmem : occurrence ∈ p.combined,
      rel.occurrenceHasUK occurrence hmem

/--
The indexed version of the final extraction spec: choose the least raw ICAO
interval that covers every UK-relevant occurrence in the reconciled plan.
-/
def LeastUKSubplan
    {Waypoint : Type u}
    {Route : Type v}
    {p : IndexedReconciledPlan Waypoint Route}
    (rel : UKRelevance p)
    (selected : RawIcaoInterval p.icao) : Prop :=
  IsLeast { interval : RawIcaoInterval p.icao | CoversUK rel interval } selected

/-- Result of deciding the low-level indexed extraction spec. -/
inductive LeastUKSubplanDecision
    {Waypoint : Type u}
    {Route : Type v}
    (p : IndexedReconciledPlan Waypoint Route)
    (rel : UKRelevance p) : Type (max u v) where
  | none :
      (noUK : ¬ HasUK rel) ->
        LeastUKSubplanDecision p rel
  | found :
      (selected : RawIcaoInterval p.icao) ->
      (hasUK : HasUK rel) ->
      LeastUKSubplan rel selected ->
        LeastUKSubplanDecision p rel

end Formal.IndexedSpec
