module Nope.Tests.ParsingTests where

import Test.Tasty
import Test.Tasty.HUnit

import Nope.Parsing
import Nope.Sources
import Nope.Results
import qualified Nope.Nodes as Nodes

parsingTestSuite :: TestTree
parsingTestSuite = testGroup "ParsingTests" [
    parsingTestCase "Unexpected char"
        "!"
        (Left (SyntaxError (SourcePoint AnonymousSource 0 0) "Unexpected character '!'")),
        
    parsingTestCase "Unexpected token"
        "print 42"
        (Left (SyntaxError (SourcePoint AnonymousSource 0 6) "Unexpected token '42'"))
    ]


parsingTestCase :: String -> String -> Result Nodes.Module -> TestTree
parsingTestCase testName programText expectedResult =
    let source = Source AnonymousSource programText
        actualResult = parseModule source
    in testCase testName $ expectedResult @=? actualResult
