module Nope.CousCous.Values where


data Value =
    None |
    BooleanValue Bool |
    IntegerValue Integer |
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
