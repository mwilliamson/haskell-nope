module Nope.Tests.ExecutionTests where

import Test.Tasty
import Test.Tasty.HUnit

import Nope
import Nope.Sources
import Nope.Results

executionTestSuite :: TestTree
executionTestSuite = testGroup "ExecutionTests" [
    programTestCase "Printing integer prints that integer to stdout"
        "print(42)"
        (Right (Stdout "42\n")),
        
    programTestCase "Variable can be referenced after it has been set"
        "x = 42\nprint(x)"
        (Right (Stdout "42\n"))
    ]


programTestCase :: [Char] -> [Char] -> Result Stdout -> TestTree
programTestCase testName programText expectedResult =
    let source = Source AnonymousSource programText
        actualResult = runProgram source
    in testCase testName $ expectedResult @=? actualResult
