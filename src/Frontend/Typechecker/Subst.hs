module Frontend.Typechecker.Subst where

import Data.Map qualified as Map
import Frontend.Typechecker.Types (MetaTy, TypeTc)
import Relude hiding (find)
import Types (TypeX(..))
import Frontend.Error (TcError)
import Control.Monad.Writer (MonadWriter)

newtype Subst = Subst {unSubst :: Map MetaTy TypeTc}
  deriving (Show)

compose :: Subst -> Subst -> Subst
compose s1@(Subst l) (Subst r) = Subst $ Map.map (apply s1) r `Map.union` l

apply :: Subst -> TypeTc -> TypeTc
apply sub ty = case ty of
  TyLitX info lit -> TyLitX info lit
  TyVarX info a -> TyVarX info a
  TyFunX info l r -> TyFunX info (apply sub l) (apply sub r)
  TypeX meta -> fromMaybe (TypeX meta) (find meta sub)

find :: MetaTy -> Subst -> Maybe TypeTc
find ty (Subst sub) = Map.lookup ty sub

nullSubst :: Subst
nullSubst = Subst mempty

singleton :: MetaTy -> TypeTc -> Subst
singleton meta ty = Subst $ Map.singleton meta ty

unify :: MonadWriter [TcError] m => TypeTc -> TypeTc -> m Subst
unify ty1 ty2 = case (ty1, ty2) of
    (TypeX meta, ty2) -> pure $ singleton meta ty2
    (ty1, TypeX meta) -> pure $ singleton meta ty1
    (TyLitX _ lit1, TyLitX _ lit2) 
      | lit1 == lit2 -> pure nullSubst
      | otherwise -> undefined
    (TyVarX _ name1, TyVarX _ name2) | name1 == name2 -> pure nullSubst
    (TyFunX _ l1 r1, TyFunX _ l2 r2) -> undefined
    (ty1, ty2) -> undefined
