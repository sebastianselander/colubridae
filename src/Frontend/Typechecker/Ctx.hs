{-# LANGUAGE TemplateHaskell #-}

module Frontend.Typechecker.Ctx where

import Control.Lens (makeLenses)
import Control.Lens.Setter (locally)
import Control.Monad.Reader (MonadReader)
import Data.Map (Map)
import Frontend.Renamer.Types (ExprRn, FnRn)
import Frontend.Typechecker.Types (TypeTc)
import Names (Ident, Names)
import Relude (Show)
import Error.Diagnose (Position)

data Ctx = Ctx
    { _functions :: Map Ident (TypeTc, Position)
    , _constructors :: Map Ident (TypeTc, Position)
    , _returnType :: TypeTc
    , _currentFun :: FnRn
    , _exprStack :: [ExprRn]
    , _names :: Names
    }
    deriving (Show)

$(makeLenses ''Ctx)

push :: (MonadReader Ctx m) => ExprRn -> m a -> m a
push expr = locally exprStack (expr :)
