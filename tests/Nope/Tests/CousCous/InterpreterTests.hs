module Nope.Tests.CousCous.InterpreterTests where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Nope.CousCous.Nodes as Nodes
import qualified Nope.CousCous.Interpreter as Interpreter


declaration = (Nodes.VariableDeclaration "x" 1)


interpreterTestSuite :: TestTree
interpreterTestSuite = testGroup "InterpreterTests" [
    programTestCase "Printing integer prints that integer to stdout" 
        [Nodes.ExpressionStatement (Nodes.Call (Nodes.builtin "print") [Nodes.Literal 42])]
        "42\n",
        
    programTestCase "Variable can be referenced after it has been set"
        [
            (Nodes.Assign (Nodes.VariableReference declaration) (Nodes.Literal 42)),
            (Nodes.ExpressionStatement (Nodes.Call (Nodes.builtin "print") [Nodes.VariableReference declaration]))
            ]
        "42\n",
        
    programTestCase "Attempting to access undefined variable raises error"
        [Nodes.ExpressionStatement (Nodes.Call (Nodes.builtin "print") [Nodes.VariableReference declaration])]
        "Exception: undefined variable: 'x'",
        
    programTestCase "Previous writes to stdout are retained when error is raised"
        [
            Nodes.ExpressionStatement (Nodes.Call (Nodes.builtin "print") [Nodes.Literal 42]),
            Nodes.ExpressionStatement (Nodes.Call (Nodes.builtin "print") [Nodes.VariableReference declaration])
            ]
        "42\nException: undefined variable: 'x'",
        
    programTestCase "Following writes to stdout are ignored when error is raised"
        [
            Nodes.ExpressionStatement (Nodes.Call (Nodes.builtin "print") [Nodes.VariableReference declaration]),
            Nodes.ExpressionStatement (Nodes.Call (Nodes.builtin "print") [Nodes.Literal 42])
            ]
        "Exception: undefined variable: 'x'"
    ]


programTestCase :: [Char] -> [Nodes.Statement] -> String -> TestTree
programTestCase testName ast expectedStdout =
    let state = (Interpreter.run (Nodes.Module ast))
    in testCase testName $ expectedStdout @=? (Interpreter.stdout state)
