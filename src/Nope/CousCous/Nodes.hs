module Nope.CousCous.Nodes where

data Expression =
    NoneLiteral |
    BooleanLiteral Bool |
    VariableReference VariableDeclaration |
    IntegerLiteral Integer |
    Call Expression [Expression]
    deriving (Show, Eq)


data Module = Module [Statement]    
    deriving (Show, Eq)

data Statement =
    ExpressionStatement Expression |
    Assign Expression Expression |
    If Expression [Statement] [Statement]
    deriving (Show, Eq)

data VariableDeclaration =
    VariableDeclaration String Int |
    Builtin String |
    Temporary Int
    deriving (Show, Eq, Ord)

variableDeclarationName :: VariableDeclaration -> String
variableDeclarationName (Builtin name) = name
variableDeclarationName (VariableDeclaration name _) = name
variableDeclarationName (Temporary declarationId) = "(tmp" ++ (show declarationId) ++ ")"

builtin :: String -> Expression
builtin name = VariableReference (Builtin name)
