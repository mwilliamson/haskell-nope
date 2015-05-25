module Nope.Tests.NameResolutionTests where

import Test.Tasty
import Test.Tasty.HUnit

import Nope.Parsing (parsedModule)
import Nope.NameDeclaration (VariableDeclaration(..))
import Nope.NameResolution
import qualified Nope.Nodes as Nope

nameResolutionTestSuite :: TestTree
nameResolutionTestSuite = testGroup "NameResolutionTests"
    [ testCase "assigned names in module are scoped to module" $
        let moduleNode = parsedModule [Nope.Assign [Nope.VariableReference "x"] Nope.NoneLiteral]
            scopedNodes = resolveReferences moduleNode
            expectedResult = [Nope.Assign [Nope.VariableReference (VariableDeclaration "x" 1)] Nope.NoneLiteral]
        in expectedResult @=? Nope.statements (resolveReferences moduleNode)
    ]
