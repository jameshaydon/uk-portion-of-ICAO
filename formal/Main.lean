import Formal.Categorical

open Formal.Categorical
open Model.Concrete
open Examples

def main : IO Unit :=
  IO.println
    (repr
      (rawResultTokens
        (ukPortionOfIcaoRaw (Point := Nat) (Route := RawRoute)
          appUKRaw appIcaoRaw appRawAdexp)))
