module Nope.Tests.TypeCheckerTests (typeCheckerTestSuite) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map

import Nope.NameResolution
import Nope.TypeChecker
import qualified Nope.Nodes as Nodes
import Nope.Tests.Variables

decl name = VariableDeclaration name (declarationId name)
ref name = Nodes.VariableReference (decl name)

typeCheckerTestSuite :: TestTree
typeCheckerTestSuite = testGroup "TypeCheckerTests"
    [ testGroup "infer"
        [ testCase "type of none literal is NoneType" $
            (success NoneType) @=? (inferWithEmptyEnvironment Nodes.none)
        
        , testCase "type of integer literal is int" $
            (success IntType) @=? (inferWithEmptyEnvironment (Nodes.integer 42))
        
        , testCase "type of reference is type of variable in environment" $
            (success IntType) @=? (infer (Map.singleton (decl "x") IntType) (ref "x"))
        ]
    ]

inferWithEmptyEnvironment :: ResolvedExpression -> Maybe Type
inferWithEmptyEnvironment = infer Map.empty


success = Just
