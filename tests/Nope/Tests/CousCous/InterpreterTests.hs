module Nope.Tests.CousCous.InterpreterTests where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Nope.CousCous.Nodes as Nodes
import qualified Nope.CousCous.Interpreter as Interpreter

interpreterTestSuite :: TestTree
interpreterTestSuite = testGroup "InterpreterTests" [
    programTestCase "Printing integer prints that integer to stdout" 
        [Nodes.ExpressionStatement (Nodes.Call (Nodes.Builtin "print") [Nodes.Literal 42])]
        "42\n",
        
    programTestCase "Variable can be referenced after it has been set"
        [
            (Nodes.Assign (Nodes.VariableReference "x") (Nodes.Literal 42)),
            (Nodes.ExpressionStatement (Nodes.Call (Nodes.Builtin "print") [Nodes.VariableReference "x"]))
            ]
        "42\n",
        
    programTestCase "Attempting to access undefined variable raises error"
        [Nodes.ExpressionStatement (Nodes.Call (Nodes.Builtin "print") [Nodes.VariableReference "x"])]
        "Exception: undefined variable: 'x'",
        
    programTestCase "Previous writes to stdout are retained when error is raised"
        [
            Nodes.ExpressionStatement (Nodes.Call (Nodes.Builtin "print") [Nodes.Literal 42]),
            Nodes.ExpressionStatement (Nodes.Call (Nodes.Builtin "print") [Nodes.VariableReference "x"])
            ]
        "42\nException: undefined variable: 'x'",
        
    programTestCase "Following writes to stdout are ignored when error is raised"
        [
            Nodes.ExpressionStatement (Nodes.Call (Nodes.Builtin "print") [Nodes.VariableReference "x"]),
            Nodes.ExpressionStatement (Nodes.Call (Nodes.Builtin "print") [Nodes.Literal 42])
            ]
        "Exception: undefined variable: 'x'"
    ]


programTestCase :: [Char] -> [Nodes.Statement] -> String -> TestTree
programTestCase testName ast expectedStdout =
    let state = (Interpreter.run (Nodes.Module ast))
    in testCase testName $ expectedStdout @=? (Interpreter.stdout state)
