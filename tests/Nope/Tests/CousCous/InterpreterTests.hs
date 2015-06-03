module Nope.Tests.CousCous.InterpreterTests (interpreterTestSuite) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Nope.CousCous.Nodes as Nodes
import qualified Nope.CousCous.Interpreter as Interpreter


declaration = Nodes.VariableDeclaration "x" 1
reference = Nodes.VariableReference declaration

boolTestCase name expression expectedBoolValue =
    let boolExpression = (Nodes.Call (Nodes.builtin "bool") [expression])
        printExpression = (Nodes.Call (Nodes.builtin "print") [boolExpression])
    in programTestCase name [Nodes.ExpressionStatement printExpression] (expectedBoolValue ++ "\n")


interpreterTestSuite :: TestTree
interpreterTestSuite = testGroup "InterpreterTests" [
    programTestCase "Printing integer prints that integer to stdout" 
        [Nodes.ExpressionStatement (Nodes.Call (Nodes.builtin "print") [Nodes.IntegerLiteral (42 :: Integer)])]
        "42\n",
        
    programTestCase "Cannot call literal" 
        [Nodes.ExpressionStatement (Nodes.Call (Nodes.NoneLiteral) [])]
        "Exception: None is not callable",
    
    testGroup "bool(value)" [
        boolTestCase "bool(None) is False" Nodes.NoneLiteral "False",
        boolTestCase "bool(False) is False" (Nodes.BooleanLiteral False) "False",
        boolTestCase "bool(True) is False" (Nodes.BooleanLiteral True) "True",
        boolTestCase "bool(0) is False" (Nodes.IntegerLiteral 0) "False",
        boolTestCase "bool(42) is True" (Nodes.IntegerLiteral 42) "True"
    ],
    
    testGroup "assignment" [
        programTestCase "Variable can be referenced after it has been set"
            [
                (Nodes.Assign reference (Nodes.IntegerLiteral 42)),
                printStatement reference
                ]
            "42\n",
        
        programTestCase "Cannot assign to a function call"
            [
                Nodes.Assign (Nodes.Call (Nodes.builtin "print") []) Nodes.NoneLiteral
                ]
            "Exception: cannot assign to function call"
    ],
    
    testGroup "if then else" [
        programTestCase "True branch is executed if condition is True" [
            (Nodes.If (Nodes.BooleanLiteral True)
                [Nodes.Assign reference (Nodes.IntegerLiteral 1)]
                [Nodes.Assign reference (Nodes.IntegerLiteral 2)]),
            printStatement reference
        ] "1\n",
        
        programTestCase "False branch is executed if condition is False" [
            (Nodes.If (Nodes.BooleanLiteral False)
                [Nodes.Assign reference (Nodes.IntegerLiteral 1)]
                [Nodes.Assign reference (Nodes.IntegerLiteral 2)]),
            printStatement reference
        ] "2\n",
        
        programTestCase "Error if condition is not boolean" [
            (Nodes.If Nodes.NoneLiteral [] [])
        ] "Exception: condition must be bool"
    ],
    
    testGroup "function definition" [
        programTestCase "Function returns None by default" [
            (Nodes.FunctionDefinition (Nodes.VariableDeclaration "f" 1) []),
            printStatement (Nodes.Call (Nodes.VariableReference (Nodes.VariableDeclaration "f" 1)) [])
        ] "None\n"
    ],
        
    programTestCase "Attempting to access undefined variable raises error"
        [printStatement reference]
        "Exception: undefined variable: 'x'",
        
    programTestCase "Previous writes to stdout are retained when error is raised"
        [
            printStatement (Nodes.IntegerLiteral 42),
            printStatement reference
            ]
        "42\nException: undefined variable: 'x'",
        
    programTestCase "Following writes to stdout are ignored when error is raised"
        [
            printStatement reference,
            printStatement (Nodes.IntegerLiteral 42)
            ]
        "Exception: undefined variable: 'x'"
    ]


programTestCase :: [Char] -> [Nodes.Statement] -> String -> TestTree
programTestCase testName ast expectedStdout =
    let state = (Interpreter.run (Nodes.Module ast))
    in testCase testName $ expectedStdout @=? (Interpreter.stdout state)


printStatement expression =
    Nodes.ExpressionStatement (Nodes.Call (Nodes.builtin "print") [expression])
