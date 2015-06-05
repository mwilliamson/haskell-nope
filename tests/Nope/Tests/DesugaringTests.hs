module Nope.Tests.DesugaringTests where

import Test.Tasty
import Test.Tasty.HUnit

import Nope.NameResolution
import Nope.Desugaring
import qualified Nope.Nodes as Nope
import qualified Nope.CousCous.Nodes as CousCous

desugaringTestSuite :: TestTree
desugaringTestSuite = testGroup "DesugaringTests" [
    desugarStatementTestCase "assignment to single variable is desugared to single assignment"
        (Nope.Assign [Nope.VariableReference (VariableDeclaration "x" 1)] Nope.none)
        [CousCous.Assign (CousCous.VariableReference (CousCous.VariableDeclaration "x" 1)) CousCous.NoneLiteral],
        
    desugarStatementTestCase "assigning same value to multiple variables is desugared to multiple assignments"
        (Nope.Assign [Nope.VariableReference (VariableDeclaration "x" 1), Nope.VariableReference (VariableDeclaration "y" 2)] Nope.none)
        [
            CousCous.Assign (CousCous.VariableReference (CousCous.Temporary 1)) CousCous.NoneLiteral,
            CousCous.Assign (CousCous.VariableReference (CousCous.VariableDeclaration "x" 1)) (CousCous.VariableReference (CousCous.Temporary 1)),
            CousCous.Assign (CousCous.VariableReference (CousCous.VariableDeclaration "y" 2)) (CousCous.VariableReference (CousCous.Temporary 1))
            ]
    ]


desugarStatementTestCase :: String -> ResolvedStatement -> [CousCous.Statement] -> TestTree
desugarStatementTestCase name nopeStatement expectedCousCous =
    let (CousCous.Module _ statements) = desugar (Nope.Module [] [nopeStatement])
    in testCase name $ expectedCousCous @=? statements
