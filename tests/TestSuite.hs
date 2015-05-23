import Test.Tasty
import Test.Tasty.HUnit

import Nope

import Nope.Tests.CousCous.InterpreterTests
import Nope.Tests.ParsingTests
import Nope.Tests.ExecutionTests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
    interpreterTestSuite,
    parsingTestSuite,
    executionTestSuite
    ]
