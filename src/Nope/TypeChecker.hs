module Nope.TypeChecker (infer, Type(..), Environment, TypeError(..)) where

import Data.List (intercalate)

import qualified Data.Map.Strict as Map

import Nope.NameResolution
import Nope.Nodes

data Type =
    NoneType |
    IntType |
    FunctionType [Type] Type
    
    deriving (Eq, Show)


describeType :: Type -> String
describeType NoneType = "NoneType"
describeType IntType = "int"
describeType (FunctionType args returnType) =
    (intercalate ", " (map describeType args)) ++ "-> " ++ (describeType returnType)


data TypeError =
    UnexpectedValueTypeError {
        unexpectedValueTypeErrorExpectedType :: String,
        unexpectedValueTypeErrorActualType :: String
    } |
    UnboundValueError
    
    deriving (Eq, Show)


type Environment = Map.Map VariableDeclaration Type

infer :: Environment -> ResolvedExpression -> Either TypeError Type

infer _ (Literal literal) = return $ inferLiteral literal

infer environment (VariableReference declaration) =
    case Map.lookup declaration environment of
        Just referenceType -> return referenceType
        Nothing -> Left UnboundValueError
    
infer environment (Call func _) = do
    functionType <- infer environment func
    case functionType of
        FunctionType _ returnType -> Right returnType
        _ -> Left $ UnexpectedValueTypeError {
            unexpectedValueTypeErrorExpectedType = "function",
            unexpectedValueTypeErrorActualType = (describeType functionType)
        }

inferLiteral :: Literal -> Type
inferLiteral NoneLiteral = NoneType
inferLiteral (IntegerLiteral _) = IntType
