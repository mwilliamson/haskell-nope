module Nope.Tests.DesugaringTests where

import Test.Tasty
import Test.Tasty.HUnit

import Nope.Desugaring
import Nope.Nodes as Nope
import Nope.CousCous.Nodes as CousCous

desugaringTestSuite :: TestTree
desugaringTestSuite = testGroup "DesugaringTests" [
    desugarStatementTestCase "assignment to single variable is desugared to single assignment"
        (Nope.Assign [Nope.VariableReference "x"] Nope.NoneLiteral)
        [CousCous.Assign (CousCous.VariableReference "x") CousCous.NoneLiteral],
        
    desugarStatementTestCase "assigning same value to multiple variables is desugared to multiple assignments"
        (Nope.Assign [Nope.VariableReference "x", Nope.VariableReference "y"] Nope.NoneLiteral)
        [
            CousCous.Assign (CousCous.VariableReference "tmp") CousCous.NoneLiteral,
            CousCous.Assign (CousCous.VariableReference "x") (CousCous.VariableReference "tmp"),
            CousCous.Assign (CousCous.VariableReference "y") (CousCous.VariableReference "tmp")
            ]
    ]


desugarStatementTestCase :: String -> Nope.Statement -> [CousCous.Statement] -> TestTree
desugarStatementTestCase name nopeStatement expectedCousCous =
    testCase name $ expectedCousCous @=? (desugarStatement nopeStatement)
