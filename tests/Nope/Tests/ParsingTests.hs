module Nope.Tests.ParsingTests where

import Data.List (isPrefixOf)

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
        (Left (SyntaxError (SourcePoint AnonymousSource 0 6) "Unexpected token '42'")),
        
    parsingTestCase "Unexpected token"
        "del x"
        (Left (SyntaxError (SourcePoint AnonymousSource 0 0) "Unsupported node: delete statement"))
    ]


parsingTestCase :: String -> String -> Result ParsedModule -> TestTree
parsingTestCase testName programText expectedResult =
    testCase testName $ expectedResult @=? (parseString programText)


parseString :: String -> Result ParsedModule
parseString programText =
    parseModule (Source AnonymousSource programText)
