module Nope.Tests.ExecutionTests where

import Test.Tasty
import Test.Tasty.HUnit

import Nope
import Nope.Sources
import Nope.Results

executionTestSuite :: TestTree
executionTestSuite = testGroup "ExecutionTests" [
    programTestCase "Printing None prints to stdout"
        "print(None)"
        (Right (Stdout "None\n")),
        
    programTestCase "Printing integer prints that integer to stdout"
        "print(42)"
        (Right (Stdout "42\n")),
        
    programTestCase "Printed expressions are separated by space"
        "print(1, 2, 3)"
        (Right (Stdout "1 2 3\n")),
        
    programTestCase "Variable can be referenced after it has been set"
        "x = 42\nprint(x)"
        (Right (Stdout "42\n")),
        
    programTestCase "Multiple variables can be set to the same value in single assignment statement"
        "x = y = 42\nprint(x, y)"
        (Right (Stdout "42 42\n")),
        
    programTestCase "Attempting to access undefined variable raises error"
        "print(x)"
        (Right (Stdout "Exception: undefined variable: 'x'")),
        
    programTestCase "Can call user-defined function"
        "def f():\n  print(42)\nf()"
        (Right (Stdout "42\n")),
        
    programTestCase "User-defined functions can return values"
        "def f():\n  return 42\nprint(f())"
        (Right (Stdout "42\n")),
        
    programTestCase "User-defined functions store locals in their own scope"
        (unlines [
            "x = 42",
            "def f():",
            "    x = 11",
            "f()",
            "print(x)"])
        (Right (Stdout "42\n")),
        
    programTestCase "User-defined functions can access variables from outer scope"
        (unlines [
            "x = 42",
            "def f():",
            "    return x",
            "print(f())"])
        (Right (Stdout "42\n")),
        
    programTestCase "Functions can receive arguments"
        (unlines [
            "def f(x):",
            "    return x",
            "print(f(42))"])
        (Right (Stdout "42\n"))
    ]


programTestCase :: [Char] -> [Char] -> Result Stdout -> TestTree
programTestCase testName programText expectedResult =
    let source = Source AnonymousSource programText
        actualResult = runProgram source
    in testCase testName $ expectedResult @=? actualResult
