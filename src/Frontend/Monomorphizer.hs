{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Frontend.Monomorphizer where

import Control.Monad (foldM)
import Data.Either.Extra (fromEither)
import Data.Foldable (foldrM)
import Data.Generics (everywhereM, mkM)
import Relude hiding (Type)

import Data.Functor qualified as Functor
import Data.Map qualified as Map
import Prettyprinter qualified as Pretty

import Frontend.MonomorphizerCollector (Item (..))
import Frontend.Substitution (Substitution (Subst), apply)
import Frontend.Typechecker.Types
    ( AdtTc,
      ConstructorTc,
      DefTc,
      ExprTc,
      FnTc,
      FnType (..),
      ModuleTc,
      NodeType (..),
      ProgramTc,
      TypeApp (TypeApp),
      TypeTc,
    )
import Frontend.Types
    ( Adt (Adt),
      Arg (Arg),
      Constructor (..),
      Def (DefAdt, DefFn, DefImport),
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
monomorphizeModule mod@(Module namespace _) items = insertMonoNode mod (deduplicate (fromMaybe [] $ Map.lookup namespace items))

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

rename :: (MonadState ([(ExplicitName, FnType)]) m) => RenameTable -> ExprTc -> m ExprTc
rename tbl expr = case expr of
    Expr (TypeApp (Var (loc, ty, bind) name) type_args nodeType) ->
        case Map.lookup (name, type_args) tbl of
            Just name -> do
                let (args, ret)
                        | TyFun NoExtField args ret <- ty = (args, ret)
                        | otherwise = __IMPOSSIBLE__
                modify ((name, FnType {tyvars = [], retType = ret, argTypes = args}) :)
                pure $ Expr (TypeApp (Var (loc, ty, bind) name) type_args nodeType)
            Nothing -> pure $ Expr (TypeApp (Var (loc, ty, bind) name) type_args nodeType)
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
insertMonoNode :: ModuleTc -> [Item] -> (ModuleTc, RenameTable)
insertMonoNode (Module namespace defs) items =
    let (monoDefs, tbl) = runState (monomorphizeDefs defs items) mempty
     in (Module namespace (defs <> monoDefs), tbl)

find_fn ::
    -- | Function we're looking for
    ExplicitName ->
    -- | All defs in the program
    [DefTc] ->
    -- | Function X we were looking for
    Maybe FnTc
find_fn name = foldr f Nothing
  where
    f :: DefTc -> Maybe FnTc -> Maybe FnTc
    f _ (Just fn) = Just fn
    f def Nothing = case def of
        DefFn fn@(Fn _ fnname _ _ _ _) | name == fnname -> Just fn
        _ -> Nothing

find_adt ::
    -- | Constructor we're looking for
    ExplicitName ->
    -- | All defs in the program
    [DefTc] ->
    -- | Type X we were looking for
    Maybe AdtTc
find_adt name = foldr f Nothing
  where
    f :: DefTc -> Maybe AdtTc -> Maybe AdtTc
    f _ (Just fn) = Just fn
    f def Nothing = case def of
        DefAdt adt@(Adt _ _ _ constructors) | any ((name ==) . conNameOf) constructors -> Just adt
        _ -> Nothing

conNameOf :: ConstructorTc -> ExplicitName
conNameOf (EnumCons _ name) = name
conNameOf (FunCons _ name _) = name

defNameOf :: DefTc -> ExplicitName
defNameOf (DefFn (Fn _ name _ _ _ _)) = name
defNameOf (DefAdt (Adt _ name _ _)) = name
defNameOf _ = __IMPOSSIBLE__

-- FIXME: We must rename constructors too, currently we get a collision for each monomorphized type
monomorphizeDefs ::
    (MonadState RenameTable m) =>
    -- | All defs
    [DefTc] ->
    -- | All items
    [Item] ->
    -- | Newly added monomorphized defs. Not including the already existing ones.
    m [DefTc]
monomorphizeDefs defs = fmap Map.elems . foldrM f mempty
  where
    f ::
        (MonadState RenameTable m) =>
        Item ->
        Map (ExplicitName, [TypeTc]) DefTc ->
        m (Map (ExplicitName, [TypeTc]) DefTc)
    f item store =
        case item.nodeType of
            Function -> case find_fn item.name defs of
                Nothing -> __IMPOSSIBLE__
                Just (Fn NoExtField name typarams args ty body) ->
                    case Map.lookup (name, item.type_args) store of
                        Just _ -> pure store
                        Nothing -> do
                            let sub =
                                    Subst
                                        $ Map.fromList
                                            ( zip
                                                (fmap (TypeVar NoExtField) (Frontend.Types.toList typarams))
                                                item.type_args
                                            )
                            let monoName = make_mono_name item.type_args name
                            modify $ Map.insert (item.name, item.type_args) monoName
                            pure
                                $ Map.insert
                                    (name, item.type_args)
                                    ( DefFn
                                        ( Fn
                                            NoExtField
                                            monoName
                                            emptyTyParamList
                                            (fmap (\(Arg _ name ty) -> Arg NoExtField name (apply sub ty)) args)
                                            (apply sub ty)
                                            (apply sub body)
                                        )
                                    )
                                    store
            Constructor -> case find_adt item.name defs of
                Nothing -> __IMPOSSIBLE__
                Just (Adt loc name typarams cons) -> case Map.lookup (name, item.type_args) store of
                    Just _ -> pure store
                    Nothing -> do
                        let sub =
                                Subst
                                    $ Map.fromList
                                        ( zip
                                            (fmap (TypeVar NoExtField) (Frontend.Types.toList typarams))
                                            item.type_args
                                        )
                        let monoName = make_mono_name item.type_args name
                        modify $ Map.insert (item.name, item.type_args) monoName
                        cons <- mapM (monomorphizeCon sub item.type_args) cons
                        pure
                            $ Map.insert
                                (name, item.type_args)
                                (DefAdt (Adt loc monoName emptyTyParamList cons))
                                store

monomorphizeCon ::
    (MonadState RenameTable m) =>
    Substitution -> [TypeTc] -> ConstructorTc -> m ConstructorTc
monomorphizeCon sub typeArgs (EnumCons (loc, ty) name) = do
    let monoName = make_mono_name typeArgs name
    modify $ Map.insert (name, typeArgs) monoName
    pure $ EnumCons (loc, apply sub ty) monoName
monomorphizeCon sub typeArgs (FunCons (loc, ty) name tys) = do
    let monoName = make_mono_name typeArgs name
    modify $ Map.insert (name, typeArgs) monoName
    pure $ FunCons (loc, apply sub ty) monoName (fmap (apply sub) tys)

-- I don't get how this works. Why do we early exit using Either?
deduplicate :: [Item] -> [Item]
deduplicate = Map.elems . fromEither . foldM f Map.empty
  where
    f acc item@(Item {name, type_args, ty = _, loc = _}) =
        case Map.lookup (name, type_args) acc of
            Just _ -> Left acc
            Nothing -> Right (Map.insert (name, type_args) item acc)
