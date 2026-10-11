{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Frontend.Monomorphizer where

import Control.Monad (foldM)
import Data.Either.Extra (fromEither)
import Data.Generics (everywhere, everywhereM, mkM, mkT)
import Data.Text.Lazy (unpack)
import Data.Tuple.Extra (uncurry3)
import Relude hiding (Type)
import Text.Pretty.Simple (pShow)

import Data.Map qualified as Map
import Prettyprinter qualified as Pretty

import Frontend.MonomorphizerCollector (Item (..))
import Frontend.Substitution (Substitution (Subst), apply)
import Frontend.Typechecker.Types (FnType (..), Tc, TypeApp (TypeApp))
import Frontend.Types
    ( Arg (Arg),
      Boundedness (Imported),
      Def (DefFn, DefImport),
      Expr (Expr, Var),
      Fn (Fn),
      Import (ImportExplicit),
      NoExtField (NoExtField),
      Program (Program),
      Type (TyFun, TypeVar),
      emptyTyParamList,
      toList,
    )
import Impossible (__IMPOSSIBLE__)
import Names (Ident, Namespace)

import Names qualified


type RenameTable = Map (Ident, [Type Tc]) Ident


monomorphize :: Program Tc -> [Item] -> (Program Tc, RenameTable)
monomorphize prg items = runState (foldM insert_mono_fn prg (deduplicate items)) Map.empty


-- FIXME(sebsel): Apply rename to imports too
apply_rename :: RenameTable -> Program Tc -> Program Tc
apply_rename tbl prg =
    let (Program namespace defs, extra_imports) = runState (everywhereM (mkM (rename tbl)) prg) []
     in Program namespace (fmap (DefImport . uncurry3 make_import) extra_imports <> defs)


make_import :: Ident -> Namespace -> FnType -> Import Tc
make_import name namespace fnType = ImportExplicit [fnType] namespace [name]


rename :: (MonadState ([(Ident, Namespace, FnType)]) m) => RenameTable -> Expr Tc -> m (Expr Tc)
rename tbl expr = case expr of
    Expr (TypeApp (Var (loc, namespace, ty, bind) name) type_args) ->
        case Map.lookup (name, type_args) tbl of
            Just name -> do
                let TyFun NoExtField args ret = ty
                when (bind == Imported)
                    $ modify ((name, namespace, FnType {tyvars = [], retType = ret, argTypes = args}) :)
                pure $ Expr (TypeApp (Var (loc, namespace, ty, bind) name) type_args)
            Nothing -> pure $ Expr (TypeApp (Var (loc, namespace, ty, bind) name) type_args)
    _ -> pure expr


make_mono_name ::
    -- | Type arguments
    [Type Tc] ->
    -- | Original name
    Ident ->
    -- \| New name
    Ident
make_mono_name type_args =
    Names.prepend "\""
        . Names.append
            ( show
                ( Pretty.angles
                    ( Pretty.concatWith
                        (Pretty.surround Pretty.comma)
                        (fmap Pretty.pretty type_args)
                    )
                    <> "\""
                )
            )


-- We can optimize this by grouping items by the function they are monomorphizing.
insert_mono_fn ::
    (MonadState RenameTable m) => Program Tc -> Item -> m (Program Tc)
insert_mono_fn (Program namespace defs) item = do
    fn <- mono_fn
    pure (Program namespace (DefFn fn : defs))
  where
    Fn NoExtField original_name typaramlist args original_type body =
        fromMaybe __IMPOSSIBLE__ (find_fn item.name defs)
    sub =
        Subst
            $ Map.fromList
                ( zip
                    (fmap (TypeVar NoExtField) (Frontend.Types.toList typaramlist))
                    item.type_args
                )
    name = make_mono_name item.type_args original_name
    mono_fn = do
        modify (Map.insert (original_name, item.type_args) name)
        pure
            $ Fn
                NoExtField
                name
                emptyTyParamList
                (fmap (\(Arg _ name ty) -> Arg NoExtField name (apply sub ty)) args)
                (apply sub original_type)
                (apply sub body)


find_fn ::
    -- | Function we're looking for
    Ident ->
    -- | All defs in the program
    [Def Tc] ->
    -- | Pair of function X we were looking for and all defs \\ function X
    Maybe (Fn Tc)
find_fn name = foldr f Nothing
  where
    f :: Def Tc -> Maybe (Fn Tc) -> Maybe (Fn Tc)
    f _ (Just fn) = Just fn
    f def Nothing = case def of
        DefFn fn@(Fn _ fnname _ _ _ _) | name == fnname -> Just fn
        _ -> Nothing


deduplicate :: [Item] -> [Item]
deduplicate = Map.elems . fromEither . foldM f Map.empty
  where
    f acc item@(Item {name, namespace = _, type_args, ty = _, loc = _}) =
        case Map.lookup (name, type_args) acc of
            Just _ -> Left acc
            Nothing -> Right (Map.insert (name, type_args) item acc)
