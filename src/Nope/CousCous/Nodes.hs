module Nope.CousCous.Nodes where

data Expression =
    VariableReference String |
    Literal Integer |
    Builtin [Char] |
    Call Expression [Expression]
    
data Module = Module [Statement]    

data Statement =
    ExpressionStatement Expression |
    Assign Expression Expression
