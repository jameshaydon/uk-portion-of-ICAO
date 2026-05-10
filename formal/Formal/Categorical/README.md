# Categorical

Specification-level Lean 4 development for the plan in
`clean-categorical.md`.

This directory gives a completed categorical formulation of UK ICAO extraction.
Plans are endpoint-indexed morphisms, sub-plans are located factorisations, and
the UK portion is specified as the least sub-plan covering all UK-relevant
atomic occurrences.

The development includes:

- an abstract specification of least UK sub-plan extraction;
- a representation-independent front/back chopping implementation;
- a proof that the implementation returns exactly the least UK sub-plan when
  one exists;
- reconciliation interfaces for ICAO, ADEXP, and combined plans;
- a concrete endpoint-indexed path model, concrete reconciliation search, and
  raw waypoint-stream entry point matching the executable app shape.

No `sorry`, `axiom`, `admit`, `opaque`, or `unsafe` declarations are used in
this directory.

## Main Targets

There are three main declarations:

- Spec: `Extraction.LeastUKSubplan`
- Implementation: `Extraction.trimFromWholeByChopping`
- Correctness theorem: the dependent return type of
  `Extraction.trimFromWholeByChopping`, which states that the selected
  sub-plan covers the UK-relevant atoms and satisfies
  `Extraction.LeastUKSubplan`

The concrete raw waypoint-stream top-level function is
`Model.Concrete.ukPortionOfIcaoRaw`. Its observable result specification is
`Model.Concrete.RawIcaoResultCorrect`, and
`Model.Concrete.ukPortionOfIcaoRaw_correct` proves that the top-level function
satisfies that contract.

## Main Files

- `Core.lean`: endpoint-indexed categories, located sub-plans,
  decompositions, and the `FreeFinitePlanSystem` interface.
- `Core/Proofs.lean`: general facts about sub-plan order and identity plans.
- `Extraction.lean`: atomic occurrences, UK relevance, coverage, and the least
  UK sub-plan specification.
- `Implementation.lean`: front/back boundary chopping and the proved extractor.
- `Reconciliation.lean`: ICAO/ADEXP/Combined reconciliation interfaces and
  exact fibre enumeration contracts.
- `Reconciliation/FrontRecursive*.lean`: front-recursive reconciliation
  candidate enumeration, with soundness, completeness, and duplicate-freedom.
- `Model.lean`: concrete endpoint-indexed path model packaged as a
  `FreeFinitePlanSystem`.
- `Concrete.lean`: concrete ICAO/ADEXP/Combined plans, concrete UK relevance,
  concrete reconciliation search, raw waypoint-stream wrapper, and executable
  examples.

The parent module `Formal.Categorical` imports the whole directory.

## Checking

From `formal/`:

```sh
lake build Formal.Categorical
```

The front-recursive reconciliation package can also be checked directly:

```sh
lake build Formal.Categorical.Reconciliation.FrontRecursive
```
