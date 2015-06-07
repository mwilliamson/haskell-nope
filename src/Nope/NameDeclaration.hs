{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Nope.NameDeclaration where

import qualified Nope.Nodes as Nodes
import Nope.Parsing (ParsedModule, ParsedStatement, ParsedFunction, ParsedExpression)


class CreatesScope a where
    declaredNames :: a -> [String]

instance CreatesScope ParsedModule where
    declaredNames = namesDeclaredInModule

instance CreatesScope ParsedFunction where
    declaredNames = namesDeclaredInFunction

namesDeclaredInModule :: ParsedModule -> [String]
namesDeclaredInModule moduleNode =
    namesDeclaredInStatements (Nodes.statements moduleNode)

namesDeclaredInFunction :: ParsedFunction -> [String]
namesDeclaredInFunction function =
    namesDeclaredInStatements (Nodes.functionBody function)

namesDeclaredInStatements :: [ParsedStatement] -> [String]
namesDeclaredInStatements statements =
    concat (map namesDeclaredInStatement statements)

namesDeclaredInStatement :: ParsedStatement -> [String]
namesDeclaredInStatement (Nodes.Assign targets _) = concat (map namesDeclaredInTarget targets)
namesDeclaredInStatement (Nodes.FunctionStatement function) = [Nodes.functionTarget function]
namesDeclaredInStatement _ = []

namesDeclaredInTarget :: ParsedExpression -> [String]
namesDeclaredInTarget (Nodes.VariableReference name) = [name]
namesDeclaredInTarget _ = []
