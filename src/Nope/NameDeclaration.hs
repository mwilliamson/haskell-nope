{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Nope.NameDeclaration where

import Nope.Nodes as Nodes
import Nope.Parsing (ParsedModule, ParsedStatement, ParsedExpression)

data Scope = Scope { _declaredNames :: [String] }

class DeclaresScope a where
    declaredScope :: a -> Scope

instance DeclaresScope Scope where
    declaredScope = id

instance DeclaresScope s => DeclaresScope (Nodes.Module s ref) where
    declaredScope = declaredScope . Nodes.scope


declaredNames :: DeclaresScope scope => scope -> [String]
declaredNames = _declaredNames . declaredScope


declareNames :: ParsedModule -> Nodes.Module Scope String
declareNames moduleNode = moduleNode {scope = scopeForModule moduleNode}

scopeForModule :: ParsedModule -> Scope
scopeForModule moduleNode = Scope $ concat (map namesDeclaredInStatement (Nodes.statements moduleNode))

namesDeclaredInStatement :: ParsedStatement -> [String]
namesDeclaredInStatement (Nodes.Assign targets _) = concat (map namesDeclaredInTarget targets)
namesDeclaredInStatement _ = []

namesDeclaredInTarget :: ParsedExpression -> [String]
namesDeclaredInTarget (Nodes.VariableReference name) = [name]
namesDeclaredInTarget _ = []
