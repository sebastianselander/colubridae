module Frontend.Renamer.SymbolTable where

import Relude

import Names (ExplicitName, Ident, Namespace)

type SymbolId = Int

type ModuleId = Int

newtype SymbolTable = SymbolTable {table :: Map (Namespace, Ident) Binding}

data Binding = ModuleBinding ModuleId | SymbolBinding SymbolId

data SymbolKind
    = FunctionSymbol ExplicitName
    | ConstructorSymbol ExplicitName
    | TypeSymbol ExplicitName
    | VariableSymbol ExplicitName
