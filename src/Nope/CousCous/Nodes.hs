module Nope.CousCous.Nodes where

data Expression =
    NoneLiteral |
    VariableReference String |
    Literal Integer |
    Builtin [Char] |
    Call Expression [Expression]
    deriving (Show, Eq)
    
data Module = Module [Statement]    
    deriving (Show, Eq)

data Statement =
    ExpressionStatement Expression |
    Assign Expression Expression
    deriving (Show, Eq)
