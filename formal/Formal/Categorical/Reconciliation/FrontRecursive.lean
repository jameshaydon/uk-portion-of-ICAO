import Formal.Categorical.Reconciliation.FrontRecursive.Base
import Formal.Categorical.Reconciliation.FrontRecursive.Sound
import Formal.Categorical.Reconciliation.FrontRecursive.Complete
import Formal.Categorical.Reconciliation.FrontRecursive.Nodup

namespace Formal.Categorical

universe u v

namespace Reconciliation

namespace FrontRecursive

variable {R : ReconciliationSystem.{u, v}}

/-- Package the recursive candidates as an exact fibre enumeration. -/
def enumeration
    (base : IdentityBaseEnumerator R)
    (frontSplits : FrontSplitEnumerator R)
    (laws : FrontRecursiveReconciliationLaws R)
    {a b : R.Point}
    (icao : R.IcaoPlan a b)
    (adexp : R.AdexpPlan a b) :
    ReconciliationFibreEnumeration R icao adexp where
  candidates := candidates base frontSplits laws icao adexp
  sound := candidates_sound base frontSplits laws icao adexp
  complete := candidates_complete base frontSplits laws icao adexp
  nodup := candidates_nodup base frontSplits laws icao adexp

end FrontRecursive

end Reconciliation

end Formal.Categorical
