{-# LANGUAGE LambdaCase #-}

module Renamer.Rn where

import Control.Monad (foldM)
import Control.Monad.Except
import Data.Set qualified as Set
import Parser.Types
import Relude
import Renamer.Error (RnError (..), onErr)
import Renamer.Monad
import Renamer.Types
import Types
import Utils (listify')

rename :: ProgramPar -> Either Text ProgramRn
rename program@(ProgramX () defs) =
  let toplevels = Set.fromList (snd <$> getDefinitions program)
   in onErr $ runGen emptyEnv emptyCtx {definitions = toplevels}
        $ ProgramX ()
        <$> mapM rnDef defs

rnDef :: DefPar -> Gen DefRn
rnDef (Fn pos name arguments returnType statements) = do
  unique arguments
  undefined

getDefinitions :: ProgramPar -> [(SourceInfo, Ident)]
getDefinitions = listify' fnName
 where
  fnName :: DefPar -> Maybe (SourceInfo, Ident)
  fnName (Fn info name _ _ _) = Just (info, name)

unique :: (MonadError RnError m) => [ArgPar] -> m ()
unique = void . foldM f mempty
 where
  f :: (MonadError RnError m) => Set Ident -> ArgPar -> m (Set Ident)
  f seen (ArgX info name _) = do
    when (Set.member name seen) (conflictingDefinitionArgument info name)
    pure (Set.insert name seen)
