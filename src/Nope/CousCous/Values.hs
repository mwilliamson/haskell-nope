module Nope.CousCous.Values where

import qualified Nope.CousCous.Nodes as Nodes


data Value =
    None |
    BooleanValue Bool |
    IntegerValue Integer |
    Function String [Nodes.Declaration] [Nodes.Declaration] [Nodes.Statement] |
    Print |
    Bool

true :: Value
true = BooleanValue True

false :: Value
false = BooleanValue False


str :: Value -> String
str None = "None"
str (BooleanValue True) = "True"
str (BooleanValue False) = "False"
str (IntegerValue value) = show value
str Print = "<built-in function print>"
str Bool = "<class 'bool'>"
str (Function name _ _ _) = "<function " ++ name ++ ">"
