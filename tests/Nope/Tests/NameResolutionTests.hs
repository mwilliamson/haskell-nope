module Nope.Tests.NameResolutionTests where

import Test.Tasty
import Test.Tasty.HUnit

import Nope.Parsing (parsedModule)
import Nope.NameResolution
import qualified Nope.Nodes as Nope

nameResolutionTestSuite :: TestTree
nameResolutionTestSuite = testGroup "NameResolutionTests"
    [ testCase "assigned names in module are scoped to module" $
        let moduleNode = parsedModule [Nope.Assign [Nope.VariableReference "x"] Nope.none]
            scopedNodes = resolveReferences moduleNode
            expectedResult = [Nope.Assign [Nope.VariableReference (VariableDeclaration "x" 1)] Nope.none]
        in expectedResult @=? Nope.statements (resolveReferences moduleNode)
    , testCase "names are assumed to be builtins if not defined" $
        let moduleNode = parsedModule [Nope.ExpressionStatement (Nope.VariableReference "x")]
            scopedNodes = resolveReferences moduleNode
            expectedResult = [Nope.ExpressionStatement (Nope.VariableReference (Builtin "x"))]
        in expectedResult @=? Nope.statements (resolveReferences moduleNode)
    ]
