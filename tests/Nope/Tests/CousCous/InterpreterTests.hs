module Nope.Tests.CousCous.InterpreterTests where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Nope.CousCous as CC
import qualified Nope.CousCous.Interpreter as Interpreter

interpreterTestSuite :: TestTree
interpreterTestSuite = testGroup "InterpreterTests" [
    programTestCase "Printing integer prints that integer to stdout" 
        [CC.expression_statement (CC.call (CC.builtin "print") [CC.literal 42])]
        (Interpreter.Stdout "42\n"),
        
    programTestCase "Variable can be referenced after it has been set"
        [
            (CC.Assign (CC.VariableReference "x") (CC.Literal 42)),
            (CC.ExpressionStatement (CC.Call (CC.Builtin "print") [CC.VariableReference "x"]))
            ]
        (Interpreter.Stdout "42\n")
    ]


programTestCase :: [Char] -> [CC.StatementNode] -> Interpreter.Stdout -> TestTree
programTestCase testName ast expectedStdout =
    let (Interpreter.InterpreterState actualStdout _) = (Interpreter.run (CC.ModuleNode ast))
    in testCase testName $ expectedStdout @=? actualStdout
