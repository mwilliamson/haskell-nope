module Nope.Nodes where

data Module scope ref  = Module {
    moduleScope :: scope,
    statements :: [Statement ref]
} deriving (Eq, Show)

data Statement ref =
    ExpressionStatement (Expression ref) |
    Assign [Expression ref] (Expression ref) |
    Function {
        functionTarget :: ref,
        functionBody :: [Statement ref]
    }
    deriving (Eq, Show)

data Expression ref =
    Literal Literal |
    Call (Expression ref) [Expression ref] |
    VariableReference ref
    deriving (Eq, Show)

data Literal =
    NoneLiteral |
    IntegerLiteral Integer
    deriving (Eq, Show)

none :: Expression a
none = Literal NoneLiteral

integer :: Integer -> Expression a
integer value = Literal (IntegerLiteral value)
