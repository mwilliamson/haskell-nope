module Nope where

data Stdout = Stdout [Char]

data Result = Result Stdout

runProgram :: [Char] -> Result
runProgram _ = Result (Stdout "42\n")
