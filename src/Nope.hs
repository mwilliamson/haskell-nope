module Nope where

import Nope.Parsing
import Nope.Desugaring
import qualified Nope.CousCous.Interpreter as Interpreter

data Stdout = Stdout String

data Result = Result Stdout

runProgram :: String -> Result
runProgram programText =
    let (Right nopeModuleNode) = parse programText
        cousCousModuleNode = desugar nopeModuleNode
        finalState = Interpreter.run cousCousModuleNode
    in Result (Stdout (Interpreter.stdout finalState))
