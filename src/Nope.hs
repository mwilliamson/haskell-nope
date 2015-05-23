module Nope where

import Nope.Parsing
import Nope.Desugaring
import Nope.Results
import qualified Nope.CousCous.Interpreter as Interpreter

data Stdout = Stdout String

runProgram :: String -> Result Stdout
runProgram programText = do
    nopeModuleNode <- parse programText
    let cousCousModuleNode = desugar nopeModuleNode
        finalState = Interpreter.run cousCousModuleNode
    Success $ Stdout (Interpreter.stdout finalState)
