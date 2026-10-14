{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Frontend.Monomorphizer where

import Control.Monad (foldM)
import Data.Either.Extra (fromEither)
import Data.Generics (everywhereM, mkM)
import Data.Traversable (for)
import Relude hiding (Type)

import Data.Functor qualified as Functor
import Data.Map qualified as Map
import Prettyprinter qualified as Pretty

import Frontend.MonomorphizerCollector (Item (..))
import Frontend.Substitution (Substitution (Subst), apply)
import Frontend.Typechecker.Types
    ( DefTc,
      ExprTc,
      FnTc,
      FnType (..),
      ImportTc,
      ModuleTc,
      ProgramTc,
      TypeApp (TypeApp),
      TypeTc,
    )
import Frontend.Types
    ( Arg (Arg),
      Boundedness (Imported),
      Def (DefFn, DefImport),
      Expr (Expr, Var),
      Fn (Fn),
      Import (ImportExplicit),
      Module (Module),
      NoExtField (NoExtField),
      Program (Program),
      Type (TyFun, TypeVar),
      emptyTyParamList,
      toList,
    )
import Impossible (__IMPOSSIBLE__)
import Names (ExplicitName, Namespace)

import Names qualified
import Names qualified as ExplicitName

type RenameTable = Map (ExplicitName, [TypeTc]) ExplicitName

monomorphize :: Map Namespace [Item] -> ProgramTc -> (ProgramTc, RenameTable)
monomorphize items (Program NoExtField mods) =
    let (mods', renameTables) = Functor.unzip (fmap (`monomorphizeModule` items) mods)
     in (Program NoExtField mods', Map.unionsWith __IMPOSSIBLE__ renameTables)

monomorphizeModule :: ModuleTc -> Map Namespace [Item] -> (ModuleTc, RenameTable)
monomorphizeModule mod@(Module namespace _) items =
    runState
        (foldM insert_mono_fn mod (deduplicate (fromMaybe [] $ Map.lookup namespace items)))
        Map.empty

-- FIXME: Ugly. Need to figure out a more sensible algorithm
apply_rename :: RenameTable -> ModuleTc -> ModuleTc
apply_rename tbl prg =
    let (Module namespace defs, extra_imports) = runState (everywhereM (mkM (rename tbl)) prg) []
     in Module namespace (fmap (renameImport extra_imports) defs)
  where
    renameImport :: [(ExplicitName, FnType)] -> DefTc -> DefTc
    renameImport names (DefImport (ImportExplicit existingFnTypes namespace existingNames)) =
        let (names', fntypes') = unzip $ filter (\(name, _) -> name.namespace == namespace) names
         in DefImport $ ImportExplicit (existingFnTypes <> fntypes') namespace (existingNames <> names')
    renameImport _ def = def

make_import :: ExplicitName -> FnType -> ImportTc
make_import name fnType = ImportExplicit [fnType] name.namespace [name]

rename :: (MonadState ([(ExplicitName, FnType)]) m) => RenameTable -> ExprTc -> m ExprTc
rename tbl expr = case expr of
    Expr (TypeApp (Var (loc, ty, bind) name) type_args) ->
        case Map.lookup (name, type_args) tbl of
            Just name -> do
                let (args, ret)
                        | TyFun NoExtField args ret <- ty = (args, ret)
                        | otherwise = __IMPOSSIBLE__
                modify ((name, FnType {tyvars = [], retType = ret, argTypes = args}) :)
                pure $ Expr (TypeApp (Var (loc, ty, bind) name) type_args)
            Nothing -> pure $ Expr (TypeApp (Var (loc, ty, bind) name) type_args)
    _ -> pure expr

make_mono_name ::
    -- | Type arguments
    [TypeTc] ->
    -- | Original name
    ExplicitName ->
    -- \| New name
    ExplicitName
make_mono_name type_args name =
    name
        { ExplicitName.name =
            Names.append
                ( show
                    ( Pretty.angles
                        ( Pretty.concatWith
                            (Pretty.surround Pretty.comma)
                            (fmap Pretty.pretty type_args)
                        )
                    )
                )
                name.name
        }

-- We can optimize this by grouping items by the function they are monomorphizing.
insert_mono_fn ::
    (MonadState RenameTable m) => ModuleTc -> Item -> m ModuleTc
insert_mono_fn (Module namespace defs) item = do
    fn <- mono_fn
    pure (Module namespace (DefFn fn : defs))
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
    ExplicitName ->
    -- | All defs in the program
    [DefTc] ->
    -- | Pair of function X we were looking for and all defs \\ function X
    Maybe FnTc
find_fn name = foldr f Nothing
  where
    f :: DefTc -> Maybe FnTc -> Maybe FnTc
    f _ (Just fn) = Just fn
    f def Nothing = case def of
        DefFn fn@(Fn _ fnname _ _ _ _) | name == fnname -> Just fn
        _ -> Nothing

-- I don't get how this works. Why do we early exit using Either?
deduplicate :: [Item] -> [Item]
deduplicate = Map.elems . fromEither . foldM f Map.empty
  where
    f acc item@(Item {name, type_args, ty = _, loc = _}) =
        case Map.lookup (name, type_args) acc of
            Just _ -> Left acc
            Nothing -> Right (Map.insert (name, type_args) item acc)
