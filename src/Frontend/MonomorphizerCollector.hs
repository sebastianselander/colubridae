{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Frontend.MonomorphizerCollector where

import Prettyprinter ((<+>))
import Relude hiding (Type)

import Data.Set qualified as Set
import Prettyprinter qualified as Pretty

import Frontend.Typechecker.Pretty ()
import Frontend.Typechecker.Types (ExprTc, ProgramTc, TypeApp (TypeApp), TypeTc)
import Frontend.Types
    ( Expr (..),
      Module (Module),
      NoExtField (NoExtField),
      Program (..),
      SourceInfo,
    )
import Names (ExplicitName (..), Namespace)
import Utils (listify')

data Collection = Collection {namespace :: Namespace, items :: Set Item}
    deriving (Eq, Ord, Show)

data Item = Item
    {name :: ExplicitName, ty :: TypeTc, type_args :: [TypeTc], loc :: SourceInfo}
    deriving (Eq, Ord, Show)

prettyItem :: Item -> Text
prettyItem item =
    show
        $ Pretty.pretty item.name.namespace
        <> "::"
        <> Pretty.pretty item.name.name
        <> Pretty.angles
            ( Pretty.concatWith (Pretty.surround (Pretty.comma <> Pretty.space))
                $ fmap Pretty.pretty item.type_args
            )
        <> ":"
        <+> Pretty.pretty item.ty
        <+> "used at"
        <+> show item.loc

collect :: ProgramTc -> NonEmpty Collection
collect (Program NoExtField mods) =
    fmap
        (\(Module namespace defs) -> Collection namespace $ Set.fromList $ listify' find_type_app defs)
        mods

find_type_app :: ExprTc -> Maybe Item
find_type_app expr = case expr of
    -- NOTE: Type application on a variable is guaranteed by the typechecker to be a toplevel function
    Expr (TypeApp (Var (loc, ty, _) name) type_args) ->
        Just (Item name ty type_args loc)
    _ -> Nothing
