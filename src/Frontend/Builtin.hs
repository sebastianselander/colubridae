{-# LANGUAGE OverloadedStrings #-}

module Frontend.Builtin where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Frontend.Renamer.Types (Rn)
import Frontend.Typechecker.Types
import Frontend.Types (NoExtField (NoExtField), SourceInfo (SourceInfo), TypeX (..), XTyFun, XTyLit)
import Names (Ident (..))
import Relude

builtInNames :: Set Ident
builtInNames = Set.fromList $ Map.keys (builtIns @Rn)

builtIns :: (XTyFun a ~ NoExtField, XTyLit a ~ NoExtField) => Map Ident (TypeX a, SourceInfo)
builtIns = Map.singleton (Ident "printInt") (TyFunX NoExtField [Int] Unit, SourceInfo Nothing "")