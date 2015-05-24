module Nope.CousCous.Values where


data Value = None | IntegerValue Integer | Print


str :: Value -> String
str None = "None"
str (IntegerValue value) = show value
str Print = "<built-in function print>"
