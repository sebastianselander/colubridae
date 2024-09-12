{-# LANGUAGE OverloadedStrings #-}

module Frontend.Builtin where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Frontend.Renamer.Types (Rn)
import Frontend.Typechecker.Types
import Frontend.Types
    ( NoExtField (NoExtField),
      TypeX (..),
      XTyFun,
      XTyLit,
    )
import Names (Ident (..))
import Relude
import Error.Diagnose qualified as Diagnose
import Data.Default qualified as Default

builtInNames :: Set Ident
builtInNames = Set.fromList $ Map.keys (builtIns @Rn)

builtIns :: (XTyFun a ~ NoExtField, XTyLit a ~ NoExtField) => Map Ident (TypeX a, Diagnose.Position)
builtIns =
    Map.fromList
        [ (Ident "printInt", (TyFunX NoExtField [Int] Unit, Default.def))
        , (Ident "printString", (TyFunX NoExtField [String] Unit, Default.def))
        ]
