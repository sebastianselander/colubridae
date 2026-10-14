module Backend.Core.Core where

import Relude

import Backend.Core.Basic (basicCore)
import Backend.Core.Breaks (removeUnreachable, simplifyBreakAndReturn)
import Backend.Core.Types

import Frontend.Typechecker.Types qualified as Tc

lowerToCore :: Tc.ProgramTc -> Program
lowerToCore =
    removeUnreachable
        . simplifyBreakAndReturn
        . basicCore
