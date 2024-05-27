module Language where

import Data.Generics
import Relude

data Expr
    = Var Ident
    | Lit Integer
    | Lam Ident Expr
    | App Expr Expr
    deriving (Show, Eq, Ord, Data)

newtype Ident = Ident Text
    deriving (Show, Eq, Ord, Data)
