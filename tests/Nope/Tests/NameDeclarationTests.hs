module Nope.Tests.NameDeclarationTests where

import Test.Tasty
import Test.Tasty.HUnit

import Nope.Nodes as Nope
import Nope.Parsing (parsedModule)
import Nope.NameDeclaration

nameDeclarationTestSuite :: TestTree
nameDeclarationTestSuite = testGroup "NameDeclarationTests"
    [ testCase "assigned names in module are scoped to module" $
        let moduleNode = parsedModule [Nope.Assign [Nope.VariableReference "x"] Nope.NoneLiteral,
                                       Nope.Assign [Nope.VariableReference "y"] Nope.NoneLiteral]
            scopedNodes = declareNames moduleNode
        in ["x", "y"] @=? (declaredNames scopedNodes)
    ]
