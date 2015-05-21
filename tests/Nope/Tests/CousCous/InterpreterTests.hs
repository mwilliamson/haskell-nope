module Nope.Tests.CousCous.InterpreterTests where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Nope.CousCous as CC
import qualified Nope.CousCous.Interpreter as Interpreter

interpreterTestSuite :: TestTree
interpreterTestSuite = testGroup "InterpreterTests" [
    programTestCase "Printing integer prints that integer to stdout" 
        (CC.expression_statement (CC.call (CC.builtin "print") [CC.literal 42]))
        (Interpreter.InterpreterState (Interpreter.Stdout "42\n"))
    ]


programTestCase :: [Char] -> CC.StatementNode -> Interpreter.InterpreterState -> TestTree
programTestCase testName ast expectedResult =
    testCase testName $ assertState expectedResult (Interpreter.run (CC.ModuleNode [ast]))


assertState :: Interpreter.InterpreterState -> Interpreter.InterpreterState -> Assertion
assertState expected actual =
    (Interpreter.stdout expected) @=? (Interpreter.stdout actual)
