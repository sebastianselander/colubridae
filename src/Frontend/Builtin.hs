{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Builtin where

import Relude hiding (Type)

import Data.Map qualified as Map
import Data.Set qualified as Set

import Frontend.Typechecker.Types (PolyType (PolyType), Tc)
import Frontend.Types
    ( Forall,
      NoExtField (NoExtField),
      SourceInfo (SourceInfo),
      TyLit (..),
      Type (..),
      emptySpan,
    )
import Names (ExplicitName (..), Ident (..), Namespace (Namespace))

newtype Builtins a = Builtins (Map ExplicitName (PolyType, SourceInfo))

deriving instance (Forall Show a) => Show (Builtins a)

names :: Builtins a -> Set ExplicitName
names (Builtins builtins) = Set.fromList (Map.keys builtins)

isBuiltin :: ExplicitName -> Builtins a -> Bool
isBuiltin name (Builtins m) = isJust $ Map.lookup name m

lookup :: ExplicitName -> Builtins a -> Maybe (PolyType, SourceInfo)
lookup name (Builtins m) = Map.lookup name m

builtins :: Builtins Tc
builtins =
    Builtins
        ( Map.fromList
            [
                ( ( EName
                        { namespace =
                            Namespace ("std" :| [])
                        , name = Ident "printInt"
                        , oldname = Ident "printInt"
                        }
                  )
                ,
                    ( PolyType []
                        $ TyFun
                            NoExtField
                            [TyLit NoExtField Int]
                            (TyLit NoExtField Unit)
                    , SourceInfo emptySpan "Built in"
                    )
                )
            ,
                ( ( EName
                        { namespace = Namespace ("std" :| [])
                        , name = Ident "printString"
                        , oldname = Ident "printString"
                        }
                  )
                ,
                    ( PolyType []
                        $ TyFun
                            NoExtField
                            [TyLit NoExtField String]
                            (TyLit NoExtField Unit)
                    , SourceInfo emptySpan "Built in"
                    )
                )
            ,
                ( ( EName
                        { namespace = Namespace ("std" :| [])
                        , name = Ident "printChar"
                        , oldname = Ident "printChar"
                        }
                  )
                ,
                    ( PolyType []
                        $ TyFun
                            NoExtField
                            [TyLit NoExtField Char]
                            (TyLit NoExtField Unit)
                    , SourceInfo emptySpan "Built in"
                    )
                )
            ]
        )
