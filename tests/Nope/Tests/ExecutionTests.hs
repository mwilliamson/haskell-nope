module Nope.Tests.ExecutionTests where

import Test.Tasty
import Test.Tasty.HUnit

import Nope
import Nope.Results

executionTestSuite :: TestTree
executionTestSuite = testGroup "ExecutionTests" [
    programTestCase "Printing integer prints that integer to stdout"
        "print(42)"
        (Right (Stdout "42\n"))
    ]


programTestCase :: [Char] -> [Char] -> Result Stdout -> TestTree
programTestCase testName programText expectedResult =
    testCase testName $ expectedResult @=? (runProgram programText)
