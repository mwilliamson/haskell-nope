module Nope where

import Nope.Parsing
import Nope.Desugaring
import Nope.Results
import Nope.Sources
import qualified Nope.CousCous.Interpreter as Interpreter

data Stdout = Stdout String
    deriving (Show, Eq)

runProgram :: Source -> Result Stdout
runProgram source = do
    nopeModuleNode <- parse source
    let cousCousModuleNode = desugar nopeModuleNode
        finalState = Interpreter.run cousCousModuleNode
    return $ Stdout (Interpreter.stdout finalState)
