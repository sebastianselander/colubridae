{-# LANGUAGE LambdaCase #-}
module Language where

import Relude

import Data.Generics
import Text.Megaparsec.Pos (SourcePos (..), sourcePosPretty)
import Data.Text (unpack)

data SynInfo = SynInfo
    { posStart :: SourcePos
    , posEnd :: SourcePos
    }
    deriving (Show, Data)

type SynExpr = Expr SynInfo

data Expr a
    = Var a Ident
    | Lit a Integer
    | Lam a Ident (Expr a)
    | Let a Ident (Expr a) (Expr a)
    | App a (Expr a) (Expr a)
    | Plus a (Expr a) (Expr a)
    | Mul a (Expr a) (Expr a)
    deriving (Show, Eq, Ord, Functor, Data)

newtype Ident = Ident Text
    deriving (Show, Eq, Ord, Data)

class Pretty a where
    pPrint :: a -> String

instance Pretty Ident where
    pPrint (Ident v) = unpack v

instance Pretty SynInfo where
    pPrint (SynInfo{posStart, posEnd}) = 
        let start = sourcePosPretty posStart
            end = sourcePosPretty posEnd
         in start <> "\n" <> end

instance Pretty String where
    pPrint = show

instance (Pretty a) => Pretty (Expr a) where
  pPrint = \case
    Var _ v -> pPrint v
    Lit _ l -> show l
    Lam _ i e -> '\\' : pPrint i <> " -> " <> pPrint e
    Let _ i e1 e2 -> "let " <> pPrint i <> " = " <> pPrint e1 <> "\n in " <> pPrint e2
    App _ l r -> pPrint l <> " " <> pPrint r
    Plus _ l r -> pPrint l <> " + " <> pPrint r
    Mul _ l r -> pPrint l <> " * " <> pPrint r

