{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Nope.NameDeclaration where

import qualified Nope.Nodes as Nodes
import Nope.Parsing (ParsedModule, ParsedStatement, ParsedExpression)

namesDeclaredInModule :: ParsedModule -> [String]
namesDeclaredInModule moduleNode =
    concat (map namesDeclaredInStatement (Nodes.statements moduleNode))

namesDeclaredInStatement :: ParsedStatement -> [String]
namesDeclaredInStatement (Nodes.Assign targets _) = concat (map namesDeclaredInTarget targets)
namesDeclaredInStatement (Nodes.FunctionStatement function) = [Nodes.functionTarget function]
namesDeclaredInStatement _ = []

namesDeclaredInTarget :: ParsedExpression -> [String]
namesDeclaredInTarget (Nodes.VariableReference name) = [name]
namesDeclaredInTarget _ = []
