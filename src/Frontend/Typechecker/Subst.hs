module Frontend.Typechecker.Subst where

import Data.Map qualified as Map
import Frontend.Typechecker.Types (MetaTy (Unsolvable), TypeTc)
import Relude
import Types (TypeX(..), SourceInfo)
import Frontend.Error
import Control.Monad.Validate (MonadValidate)

newtype Subst = Subst {unSubst :: Map MetaTy TypeTc}
  deriving (Show)

compose :: Subst -> Subst -> Subst
compose s1@(Subst l) (Subst r) = Subst $ Map.map (apply s1) r `Map.union` l

apply :: Subst -> TypeTc -> TypeTc
apply sub ty = case ty of
  TyLitX info lit -> TyLitX info lit
  TyVarX info a -> TyVarX info a
  TyFunX info l r -> TyFunX info (apply sub l) (apply sub r)
  TypeX meta -> fromMaybe (TypeX meta) (findSubst meta sub)

findSubst :: MetaTy -> Subst -> Maybe TypeTc
findSubst ty (Subst sub) = Map.lookup ty sub

nullSubst :: Subst
nullSubst = Subst mempty

singleton :: MetaTy -> TypeTc -> Subst
singleton meta ty = Subst $ Map.singleton meta ty

unify :: MonadValidate [TcError] m => SourceInfo -> TypeTc -> TypeTc -> m Subst
unify info ty1 ty2 = case (ty1, ty2) of
    (TypeX Unsolvable, _) -> doneTcError
    (_, TypeX Unsolvable) -> doneTcError
    (TypeX meta, ty2) -> pure $ singleton meta ty2
    (ty1, TypeX meta) -> pure $ singleton meta ty1
    (TyLitX _ lit1, TyLitX _ lit2) 
      | lit1 == lit2 -> pure nullSubst
      | otherwise -> tyExpectedGot info [ty1] ty2 >> pure nullSubst
    (TyVarX _ name1, TyVarX _ name2) 
      | name1 == name2 -> pure nullSubst
      | otherwise -> tyExpectedGot info [ty1] ty2 >> pure nullSubst
    (TyFunX _ l1 r1, TyFunX _ l2 r2) -> do
        sub1 <- unify info l1 l2
        sub2 <- unify info (apply sub1 r1) (apply sub1 r2)
        pure $ sub2 `compose` sub1
    (ty1, ty2) -> tyExpectedGot info [ty1] ty2 >> pure nullSubst
