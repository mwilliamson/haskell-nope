module Nope.TypeChecker (infer, Type(..), Environment) where

import qualified Data.Map.Strict as Map

import Nope.NameResolution
import Nope.Nodes

data Type =
    NoneType |
    IntType
    
    deriving (Eq, Show)


type Environment = Map.Map VariableDeclaration Type

infer :: Environment -> ResolvedExpression -> Maybe Type
infer _ (Literal literal) = Just $ inferLiteral literal
infer environment (VariableReference declaration) =
    Map.lookup declaration environment
infer _ _ = Nothing

inferLiteral :: Literal -> Type
inferLiteral NoneLiteral = NoneType
inferLiteral (IntegerLiteral _) = IntType
