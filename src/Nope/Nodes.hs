module Nope.Nodes where

data Module = Module [Statement]
    deriving (Eq, Show)

data Statement =
    ExpressionStatement Expression |
    Assign [Expression] Expression
    deriving (Eq, Show)

data Expression =
    NoneLiteral |
    Literal Integer |
    Builtin String |
    Call Expression [Expression] |
    VariableReference String
    deriving (Eq, Show)
