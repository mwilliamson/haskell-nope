{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Nope.NameDeclaration where

import Control.Monad.State (State, get, put, evalState)
import qualified Data.Map.Strict as Map

import qualified Nope.Nodes as Nodes
import Nope.Parsing (ParsedModule, ParsedStatement, ParsedExpression)

data VariableDeclaration = VariableDeclaration String Int
    deriving (Show, Eq)
data Scope = Scope { _declarations :: Map.Map String VariableDeclaration }

class DeclaresScope a where
    declaredScope :: a -> Scope

instance DeclaresScope Scope where
    declaredScope = id

instance DeclaresScope s => DeclaresScope (Nodes.Module s ref) where
    declaredScope = declaredScope . Nodes.scope


declaredNames :: DeclaresScope scope => scope -> [String]
declaredNames = Map.keys . _declarations . declaredScope

findNames :: ParsedModule -> Nodes.Module [String] String
findNames moduleNode = moduleNode {Nodes.scope = namesDeclaredInModule moduleNode}

namesDeclaredInModule :: ParsedModule -> [String]
namesDeclaredInModule moduleNode =
    concat (map namesDeclaredInStatement (Nodes.statements moduleNode))

namesDeclaredInStatement :: ParsedStatement -> [String]
namesDeclaredInStatement (Nodes.Assign targets _) = concat (map namesDeclaredInTarget targets)
namesDeclaredInStatement _ = []

namesDeclaredInTarget :: ParsedExpression -> [String]
namesDeclaredInTarget (Nodes.VariableReference name) = [name]
namesDeclaredInTarget _ = []

type Counter = State Int

declareNames :: ParsedModule -> Nodes.Module Scope String
declareNames moduleNode =
    let scope = evalState (scopeForModule (findNames moduleNode)) 1
    in moduleNode {Nodes.scope = scope}

scopeForModule :: Nodes.Module [String] a -> Counter Scope
scopeForModule moduleNode = do
    let names = Nodes.scope moduleNode
    declarations <- namesToDeclarations names
    return $ Scope (Map.fromList declarations)

namesToDeclarations :: [String] -> Counter [(String, VariableDeclaration)]
namesToDeclarations names = mapM nameToDeclaration names

nameToDeclaration :: String -> Counter (String, VariableDeclaration)
nameToDeclaration name = do
    count <- get
    put (count + 1)
    return $ (name, VariableDeclaration name count)

declaration :: DeclaresScope a => String -> a -> Maybe VariableDeclaration
declaration name scope = Map.lookup name (_declarations (declaredScope scope))
