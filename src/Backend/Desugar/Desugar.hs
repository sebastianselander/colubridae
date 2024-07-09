module Backend.Desugar.Desugar where
import Backend.Desugar.Breaks (simplifyBreakAndReturn, removeUnreachable)
import qualified Frontend.Typechecker.Types as Tc
import Backend.Desugar.Types
import Relude
import Backend.Desugar.Basic (basicDesugar)
import Names (Names)

desugar :: Names -> Tc.ProgramTc -> Program
desugar names = removeUnreachable . simplifyBreakAndReturn . basicDesugar names
