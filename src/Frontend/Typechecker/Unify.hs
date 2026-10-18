module Frontend.Typechecker.Unify where

import Control.Lens.Getter (view)
import Control.Monad.Validate (MonadValidate)
import Relude hiding (Type)

import Frontend.Error (TcError, tyExpectedGot')
import Frontend.Renamer.Types (ArgRn, TypeRn)
import Frontend.Substitution (Substitute (apply), Substitution)
import Frontend.Typechecker.Ctx (Ctx)
import Frontend.Typechecker.Polytype (occurs)
import Frontend.Typechecker.Types
    ( ArgTc,
      BlockTc,
      ExprTc,
      LamArgTc,
      MatchArmTc,
      MetaTy (..),
      StmtTc,
      TypeApp (TypeApp),
      TypeTc,
      stmtType,
    )
import Frontend.Types
    ( Arg (..),
      Block (..),
      Expr (..),
      LamArg (..),
      MatchArm (..),
      NoExtField (NoExtField),
      SourceInfo,
      Stmt (SExpr),
      Type (..),
    )

import Frontend.Substitution qualified as Sub

-- | Unify two types. The first argument type *must* be the expected one!
unify ::
    (MonadValidate [TcError] m, MonadReader Ctx m) =>
    SourceInfo ->
    TypeTc ->
    TypeTc ->
    m Substitution
unify info ty1 ty2 = case (ty1, ty2) of
    (TyLit NoExtField lit1, TyLit NoExtField lit2)
        | lit1 == lit2 -> pure Sub.empty
        | otherwise -> tyExpectedGot' info [ty1] ty2
    (TypeVar NoExtField tvar1, TypeVar NoExtField tvar2)
        | tvar1 == tvar2 -> pure Sub.empty
        | otherwise -> tyExpectedGot' info [ty1] ty2
    (TyFun NoExtField l1 r1, TyFun NoExtField l2 r2) -> do
        unless (length l1 == length l2) (tyExpectedGot' info [ty1] ty2)
        sub1 <- unifies info (zip l1 l2)
        sub2 <- unify info (apply sub1 r1) (apply sub1 r2)
        pure $ Sub.compose sub2 sub1
    (Type (Mono mono), t) ->
        if occurs mono t then tyExpectedGot' info [ty1] ty2 else pure $ Sub.singleton (Type (Mono mono)) t
    (t, Type (Mono mono)) ->
        if occurs mono t then tyExpectedGot' info [ty1] ty2 else pure $ Sub.singleton (Type (Mono mono)) t
    (Type AnyX, _) -> pure Sub.empty
    (_, Type AnyX) -> pure Sub.empty
    (TyCon NoExtField name1 params1, TyCon NoExtField name2 params2)
        | name1 == name2 -> unifies info (zip params1 params2)
        | otherwise -> tyExpectedGot' info [ty1] ty2
    (ty1, ty2) -> tyExpectedGot' info [ty1] ty2

unifies ::
    (MonadValidate [TcError] m, MonadReader Ctx m) =>
    SourceInfo -> [(TypeTc, TypeTc)] -> m Substitution
unifies loc xs = go Sub.empty xs
  where
    go sub1 [] = pure sub1
    go sub1 ((ty1, ty2) : xs) = do
        sub2 <- unify loc (apply sub1 ty1) (apply sub1 ty2)
        go (Sub.compose sub2 sub1) xs

class TypeOf a where
    typeOf :: a -> TypeTc

instance TypeOf TypeTc where
    typeOf ty = ty

instance TypeOf StmtTc where
    typeOf = \case
        SExpr _ expr -> typeOf expr

instance TypeOf ExprTc where
    typeOf = \case
        Lit ty _ -> snd ty
        Var (_, ty, _) _ -> ty
        Prefix ty _ _ -> snd ty
        BinOp ty _ _ _ -> snd ty
        App ty _ _ -> snd ty
        Let rec _ _ -> view stmtType rec
        Ass (rec, _) _ _ _ -> view stmtType rec
        Ret ty _ -> snd ty
        EBlock _ block -> typeOf block
        Break ty _ -> snd ty
        If ty _ _ _ -> snd ty
        While ty _ _ -> snd ty
        Loop ty _ -> snd ty
        Lam ty _ _ -> snd ty
        Match ty _ _ -> snd ty
        Expr (TypeApp expr _ _) -> typeOf expr

instance TypeOf BlockTc where
    typeOf (Block ty _ _) = snd ty

instance TypeOf TypeRn where
    typeOf = \case
        TyLit NoExtField b -> TyLit NoExtField b
        TyFun NoExtField b c -> TyFun NoExtField (fmap typeOf b) (typeOf c) -- NOTE: is `emptyTyParamList` correct?
        TyCon NoExtField b params -> TyCon NoExtField b (fmap typeOf params)
        TypeVar NoExtField b -> TypeVar NoExtField b

instance TypeOf LamArgTc where
    typeOf (LamArg ty _) = ty

instance TypeOf MatchArmTc where
    typeOf (MatchArm _ _ body) = typeOf body

instance TypeOf ArgTc where
    typeOf (Arg _ _ ty) = ty

instance TypeOf ArgRn where
    typeOf (Arg _ _ ty) = typeOf ty
