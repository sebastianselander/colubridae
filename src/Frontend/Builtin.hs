{-# LANGUAGE OverloadedStrings #-}

module Frontend.Builtin where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Frontend.Renamer.Types (Rn)
import Frontend.Typechecker.Types
import Frontend.Types
    ( NoExtField (NoExtField),
      SourceInfo (SourceInfo),
      Type (..),
      XTyFun,
      XTyLit,
      Type(..),
      emptySpan, TyLit (..),
    )
import Names (Ident (..))
import Relude hiding (Type)

builtInNames :: Set Ident
builtInNames = Set.fromList $ Map.keys (builtIns @Rn)

builtIns :: (XTyFun a ~ NoExtField, XTyLit a ~ NoExtField) => Map Ident (Type a, SourceInfo)
builtIns =
    Map.fromList
        [ (Ident "printInt", (TyFun NoExtField [TyLit NoExtField Int] (TyLit NoExtField Unit), SourceInfo emptySpan "Built in"))
        , (Ident "printString", (TyFun NoExtField [TyLit NoExtField String] (TyLit NoExtField Unit), SourceInfo emptySpan "Built in"))
        ]
