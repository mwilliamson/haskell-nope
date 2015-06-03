module Nope.Tests.CousCous.InterpreterTests (interpreterTestSuite) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Nope.CousCous.Nodes as Nodes
import qualified Nope.CousCous.Interpreter as Interpreter


declaration = (Nodes.VariableDeclaration "x" 1)


boolTestCase name expression expectedBoolValue =
    let boolExpression = (Nodes.Call (Nodes.builtin "bool") [expression])
        printExpression = (Nodes.Call (Nodes.builtin "print") [boolExpression])
    in programTestCase name [Nodes.ExpressionStatement printExpression] (expectedBoolValue ++ "\n")


interpreterTestSuite :: TestTree
interpreterTestSuite = testGroup "InterpreterTests" [
    programTestCase "Printing integer prints that integer to stdout" 
        [Nodes.ExpressionStatement (Nodes.Call (Nodes.builtin "print") [Nodes.Literal 42])]
        "42\n",
        
    programTestCase "Cannot call literal" 
        [Nodes.ExpressionStatement (Nodes.Call (Nodes.NoneLiteral) [])]
        "Exception: None is not callable",
    
    testGroup "bool(value)" [
        boolTestCase "bool(None) is False" Nodes.NoneLiteral "False",
        boolTestCase "bool(False) is False" (Nodes.BooleanLiteral False) "False",
        boolTestCase "bool(True) is False" (Nodes.BooleanLiteral True) "True",
        boolTestCase "bool(0) is False" (Nodes.Literal 0) "False",
        boolTestCase "bool(42) is True" (Nodes.Literal 42) "True"
    ],
    
    testGroup "assignment" [
        programTestCase "Variable can be referenced after it has been set"
            [
                (Nodes.Assign (Nodes.VariableReference declaration) (Nodes.Literal 42)),
                (Nodes.ExpressionStatement (Nodes.Call (Nodes.builtin "print") [Nodes.VariableReference declaration]))
                ]
            "42\n",
        
        programTestCase "Cannot assign to a function call"
            [
                Nodes.Assign (Nodes.Call (Nodes.builtin "print") []) Nodes.NoneLiteral
                ]
            "Exception: cannot assign to function call"
    ],
        
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
