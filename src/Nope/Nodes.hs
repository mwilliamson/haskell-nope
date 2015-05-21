module Nope.Nodes where

data Module = Module [Statement]

data Statement = ExpressionStatement Expression

data Expression =
    Literal Integer |
    Builtin String |
    Call Expression [Expression]
