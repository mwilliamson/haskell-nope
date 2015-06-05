module Nope where

import Nope.Parsing
import Nope.NameResolution
import Nope.Desugaring
import Nope.Results
import Nope.Sources
import qualified Nope.CousCous.Interpreter as Interpreter

data Stdout = Stdout String
    deriving (Show, Eq)

runProgram :: Source -> Result Stdout
runProgram source = do
    nopeModuleNode <- parseModule source
    let resolvedModule = resolveReferences nopeModuleNode
    let cousCousModuleNode = desugar resolvedModule
        finalState = Interpreter.run cousCousModuleNode
    return $ Stdout (Interpreter.interpreterStateStdout finalState)
