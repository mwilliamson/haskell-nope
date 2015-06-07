module Nope.CousCous.Nodes where

data Expression =
    NoneLiteral |
    BooleanLiteral Bool |
    VariableReference Declaration |
    IntegerLiteral Integer |
    Call Expression [Expression]
    deriving (Show, Eq)


data Module = Module [Declaration] [Statement]
    deriving (Show, Eq)

data Statement =
    ExpressionStatement Expression |
    Assign Expression Expression |
    If Expression [Statement] [Statement] |
    Function {
        functionDeclaration :: Declaration,
        functionArguments :: [Declaration],
        functionLocalDeclarations :: [Declaration],
        functionBody :: [Statement]
    } |
    Return Expression
    deriving (Show, Eq)

data Declaration =
    VariableDeclaration String Int |
    Builtin String |
    Temporary Int
    deriving (Show, Eq, Ord)

declarationName :: Declaration -> String
declarationName (Builtin name) = name
declarationName (VariableDeclaration name _) = name
declarationName (Temporary declarationId) = "(tmp" ++ (show declarationId) ++ ")"

builtin :: String -> Expression
builtin name = VariableReference (Builtin name)
