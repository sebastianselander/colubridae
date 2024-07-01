module Backend.Desugar.Desugar where
import Backend.Desugar.Breaks (simplifyBreakAndReturn)
import qualified Frontend.Typechecker.Types as Tc
import Backend.Desugar.Types
import Relude
import Data.Generics (everywhere, mkT)
import Backend.Desugar.Basic (basicDesugar)
import Names (Names)

desugar :: Names -> Tc.ProgramTc -> Program
desugar names = everywhere (mkT simplifyBreakAndReturn) . basicDesugar names
