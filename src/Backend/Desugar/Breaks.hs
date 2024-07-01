module Backend.Desugar.Breaks where

import Backend.Desugar.Types
import Relude

simplifyBreakAndReturn :: TyExpr -> TyExpr
simplifyBreakAndReturn (Typed ty expr) = case expr of
    If condition@(Typed ty (Break breakExpr)) true false -> condition
    e -> Typed ty e
