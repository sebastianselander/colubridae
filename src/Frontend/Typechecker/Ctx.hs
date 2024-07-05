{-# LANGUAGE TemplateHaskell #-}

module Frontend.Typechecker.Ctx where

import Control.Lens (makeLenses)
import Data.Map (Map)
import Frontend.Renamer.Types (DefRn, ExprRn)
import Frontend.Typechecker.Types (TypeTc)
import Frontend.Types (SourceInfo)
import Names (Ident, Names)
import Relude (Show)
import Control.Monad.Reader (MonadReader)
import Control.Lens.Setter (locally)

data Ctx = Ctx
    { _functions :: Map Ident (TypeTc, SourceInfo)
    , _returnType :: TypeTc
    , _currentFun :: DefRn
    , _exprStack :: [ExprRn]
    , _names :: Names
    }
    deriving (Show)

$(makeLenses ''Ctx)

push :: MonadReader Ctx m => ExprRn -> m a -> m a
push expr = locally exprStack (expr :)
