module Backend.Desugar.Breaks where

import Backend.Desugar.Types

simplifyBreakAndReturn :: TyExpr -> TyExpr
simplifyBreakAndReturn (Typed ty expr) = case expr of
    If condition@(Typed _ty Break) _true _false -> condition
    If condition@(Typed _ty (Return _)) _true _false -> condition
    While condition@(Typed _ty Break) _body -> condition
    While condition@(Typed _ty (Return _)) _body -> condition
    e -> Typed ty e
