module Nope.Tests.ExecutionTests where

import Test.Tasty
import Test.Tasty.HUnit

import Nope
import Nope.Tests.Results

executionTestSuite :: TestTree
executionTestSuite = testGroup "ExecutionTests" [
    programTestCase "Printing integer prints that integer to stdout"
        "print(42)"
        (Result (Stdout "42\n"))
    ]


programTestCase :: [Char] -> [Char] -> Result -> TestTree
programTestCase testName programText expectedResult =
    testCase testName $ assertResult expectedResult (runProgram programText)
