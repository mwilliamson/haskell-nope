module Nope.Tests.CousCous.InterpreterTests where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Nope.CousCous as CC
import qualified Nope.CousCous.Interpreter as Interpreter
import Nope
import Nope.Tests.Results

interpreterTestSuite :: TestTree
interpreterTestSuite = testGroup "InterpreterTests" [
    programTestCase "Printing integer prints that integer to stdout" 
        (CC.expression_statement (CC.call (CC.builtin "print") [CC.literal 42]))
        (Result (Stdout "42\n"))
    ]


programTestCase :: [Char] -> CC.StatementNode -> Result -> TestTree
programTestCase testName ast expectedResult =
    testCase testName $ assertResult expectedResult (Interpreter.run ast)
