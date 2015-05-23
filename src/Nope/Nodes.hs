module Nope.Nodes where

data Module = Module [Statement]
    deriving (Eq, Show)

data Statement = ExpressionStatement Expression
    deriving (Eq, Show)

data Expression =
    Literal Integer |
    Builtin String |
    Call Expression [Expression]
    deriving (Eq, Show)
