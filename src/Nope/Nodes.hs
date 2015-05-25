module Nope.Nodes where

data Module ref = Module [Statement ref]
    deriving (Eq, Show)

data Statement ref =
    ExpressionStatement (Expression ref) |
    Assign [Expression ref] (Expression ref)
    deriving (Eq, Show)

data Expression ref =
    NoneLiteral |
    Literal Integer |
    Builtin String |
    Call (Expression ref) [Expression ref] |
    VariableReference ref
    deriving (Eq, Show)
