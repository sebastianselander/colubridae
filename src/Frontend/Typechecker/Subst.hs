module Frontend.Typechecker.Subst where

import Data.Map qualified as Map
import Frontend.Typechecker.Types
import Relude hiding (Any)
import Frontend.Types

newtype Subst = Subst {unSubst :: Map MetaTy TypeTc}
    deriving (Show)

class Substitution a where
    apply :: Subst -> a -> a

instance Substitution (a, TypeTc) where
  apply sub = second (apply sub)

instance (Substitution a) => Substitution [a] where
  apply sub = fmap (apply sub)

instance Substitution LamArgTc where
  apply sub (LamArgX ty name) = LamArgX (apply sub ty) name

instance Substitution TypeTc where
    apply :: Subst -> TypeTc -> TypeTc
    apply sub ty = case ty of
        TyLitX info lit -> TyLitX info lit
        TyFunX info l r -> TyFunX info (fmap (apply sub) l) (apply sub r)
        TypeX AnyX -> Any
        TypeX (MutableX ty) -> Mut (apply sub ty)

instance Substitution ExprTc where
    apply :: Subst -> ExprTc -> ExprTc
    apply sub expr = case expr of
        LitX ty lit -> LitX (apply sub ty) lit
        VarX (info, ty, bound) var -> VarX (info, apply sub ty, bound) var
        BinOpX ty l op r -> BinOpX (apply sub ty) (apply sub l) op (apply sub r)
        PrefixX ty op expr -> PrefixX (apply sub ty) op (apply sub expr)
        AppX ty l r -> AppX (apply sub ty) (apply sub l) (fmap (apply sub) r)
        LetX (StmtType ty1 ty2 info) name expr -> LetX (StmtType (apply sub ty1) (apply sub ty2) info) name (apply sub expr)
        AssX (StmtType ty1 ty2 info, bound) name op expr -> AssX (StmtType (apply sub ty1) (apply sub ty2) info, bound) name op (apply sub expr)
        RetX ty mbExpr -> RetX (apply sub ty) (fmap (apply sub) mbExpr)
        EBlockX NoExtField block -> EBlockX NoExtField (apply sub block)
        BreakX ty mbExpr -> BreakX (apply sub ty) (fmap (apply sub) mbExpr)
        IfX ty condition true false ->
            IfX (apply sub ty) (apply sub condition) (apply sub true) (fmap (apply sub) false)
        WhileX ty condition block -> WhileX (apply sub ty) (apply sub condition) (apply sub block)
        LoopX ty block -> LoopX (apply sub ty) (apply sub block)
        LamX ty args body -> LamX (apply sub ty) (apply sub args) (apply sub body)

instance Substitution StmtTc where
    apply sub stmt = case stmt of
        SExprX NoExtField expr -> SExprX NoExtField (apply sub expr)

instance Substitution BlockTc where
    apply sub (BlockX ty stmts tail) =
        BlockX (apply sub ty) (fmap (apply sub) stmts) (fmap (apply sub) tail)

compose :: Subst -> Subst -> Subst
compose s1@(Subst l) (Subst r) = Subst $ Map.map (apply s1) r `Map.union` l

findSubst :: MetaTy -> Subst -> Maybe TypeTc
findSubst ty (Subst sub) = Map.lookup ty sub

nullSubst :: Subst
nullSubst = Subst mempty

singleton :: MetaTy -> TypeTc -> Subst
singleton meta ty = Subst $ Map.singleton meta ty
