module Nope.Tests.CousCous.InterpreterTests (interpreterTestSuite) where

import Data.Char (ord)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Nope.CousCous.Nodes as Nodes
import qualified Nope.CousCous.Interpreter as Interpreter


declaration = decl "x"
reference = Nodes.VariableReference declaration

decl [name] = Nodes.VariableDeclaration [name] (ord name)
ref name = Nodes.VariableReference (decl name)

boolTestCase name expression expectedBoolValue =
    let boolExpression = (Nodes.Call (Nodes.builtin "bool") [expression])
        printExpression = (Nodes.Call (Nodes.builtin "print") [boolExpression])
    in statementsTestCase name [Nodes.ExpressionStatement printExpression] (expectedBoolValue ++ "\n")


interpreterTestSuite :: TestTree
interpreterTestSuite = testGroup "InterpreterTests" [

    statementsTestCase "Empty program does nothing" 
        []
        "",
        
    statementsTestCase "Printing integer prints that integer to stdout" 
        [Nodes.ExpressionStatement (Nodes.Call (Nodes.builtin "print") [Nodes.IntegerLiteral (42 :: Integer)])]
        "42\n",
        
    statementsTestCase "Cannot call literal" 
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
        moduleTestCase "Variable can be referenced after it has been set"
            (Nodes.Module
                [declaration]
                [
                    (Nodes.Assign reference (Nodes.IntegerLiteral 42)),
                    printStatement reference
                ])
            "42\n",
        
        statementsTestCase "Cannot assign to a function call"
            [Nodes.Assign (Nodes.Call (Nodes.builtin "print") []) Nodes.NoneLiteral]
            "Exception: cannot assign to function call"
    ],
    
    testGroup "if then else" [
        moduleTestCase "True branch is executed if condition is True"
            (Nodes.Module
                [declaration]
                [
                    (Nodes.If (Nodes.BooleanLiteral True)
                        [Nodes.Assign reference (Nodes.IntegerLiteral 1)]
                        [Nodes.Assign reference (Nodes.IntegerLiteral 2)]),
                    printStatement reference
                ])
            "1\n",
        
        moduleTestCase "False branch is executed if condition is False"
            (Nodes.Module
                [declaration]
                [
                    (Nodes.If (Nodes.BooleanLiteral False)
                        [Nodes.Assign reference (Nodes.IntegerLiteral 1)]
                        [Nodes.Assign reference (Nodes.IntegerLiteral 2)]),
                    printStatement reference
                ])
            "2\n",
        
        statementsTestCase "Error if condition is not boolean" [
            (Nodes.If Nodes.NoneLiteral [] [])
        ] "Exception: condition must be bool"
    ],
    
    testGroup "function definition" [
        statementsTestCase "Function returns None by default" [
            (Nodes.FunctionDefinition (decl "f") [] []),
            printStatement (Nodes.Call (ref "f") [])
        ] "None\n",
        
        statementsTestCase "Function returns value in return statement" [
            (Nodes.FunctionDefinition (decl "f") [] [
                Nodes.Return (Nodes.IntegerLiteral 42)
            ]),
            printStatement (Nodes.Call (ref "f") [])
        ] "42\n",
        
        statementsTestCase "Statements before return are executed" [
            (Nodes.FunctionDefinition (decl "f") [] [
                printStatement (Nodes.IntegerLiteral 42),
                Nodes.Return Nodes.NoneLiteral
            ]),
            Nodes.ExpressionStatement (Nodes.Call (ref "f") [])
        ] "42\n",
        
        statementsTestCase "Statements after return are not executed" [
            (Nodes.FunctionDefinition (decl "f") [] [
                Nodes.Return Nodes.NoneLiteral,
                printStatement (Nodes.IntegerLiteral 42)
            ]),
            Nodes.ExpressionStatement (Nodes.Call (ref "f") [])
        ] "",
        
        moduleTestCase "Assignments in function don't affect outer scope if variable is scoped to function"
            (Nodes.Module [decl "x", decl "f"] [
                Nodes.Assign (ref "x") (Nodes.IntegerLiteral 42),
                (Nodes.FunctionDefinition (decl "f") [decl "x"] [
                    Nodes.Assign (ref "x") (Nodes.IntegerLiteral 24)
                ]),
                Nodes.ExpressionStatement (Nodes.Call (ref "f") []),
                printStatement (ref "x")
            ]) "42\n",
        
        moduleTestCase "Assignments in function affect outer scope if variable is not scoped to function"
            (Nodes.Module [decl "x", decl "f"] [
                Nodes.Assign (ref "x") (Nodes.IntegerLiteral 42),
                (Nodes.FunctionDefinition (decl "f") [] [
                    Nodes.Assign (ref "x") (Nodes.IntegerLiteral 24)
                ]),
                Nodes.ExpressionStatement (Nodes.Call (ref "f") []),
                printStatement (ref "x")
            ]) "24\n"
    ],
    
    statementsTestCase "Module-level returns are illegal" [
        Nodes.Return Nodes.NoneLiteral
    ] "Exception: return outside of function",
        
    statementsTestCase "Attempting to access undefined variable raises error"
        [printStatement reference]
        "Exception: undefined variable: 'x'",
        
    statementsTestCase "Previous writes to stdout are retained when error is raised"
        [
            printStatement (Nodes.IntegerLiteral 42),
            printStatement reference
            ]
        "42\nException: undefined variable: 'x'",
        
    statementsTestCase "Following writes to stdout are ignored when error is raised"
        [
            printStatement reference,
            printStatement (Nodes.IntegerLiteral 42)
            ]
        "Exception: undefined variable: 'x'"
    ]


moduleTestCase :: String -> Nodes.Module -> String -> TestTree
moduleTestCase testName moduleNode expectedStdout =
    let state = (Interpreter.run moduleNode)
    in testCase testName $ expectedStdout @=? (Interpreter.interpreterStateStdout state)


statementsTestCase :: [Char] -> [Nodes.Statement] -> String -> TestTree
-- TODO: the decl "f" is a bit of a hack
statementsTestCase testName ast = moduleTestCase testName (Nodes.Module [decl "f"] ast)


printStatement expression =
    Nodes.ExpressionStatement (Nodes.Call (Nodes.builtin "print") [expression])
