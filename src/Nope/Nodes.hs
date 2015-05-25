module Nope.Nodes where

data Module scope ref  = Module {
    statements :: [Statement ref],
    scope :: scope }
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
