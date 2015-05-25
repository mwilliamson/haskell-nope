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
    Literal Literal |
    Call (Expression ref) [Expression ref] |
    VariableReference ref
    deriving (Eq, Show)

data Literal =
    NoneLiteral |
    IntegerLiteral Integer |
    Builtin String
    deriving (Eq, Show)

none :: Expression a
none = Literal NoneLiteral

integer :: Integer -> Expression a
integer value = Literal (IntegerLiteral value)
