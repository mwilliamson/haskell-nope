import Test.Tasty
import Test.Tasty.HUnit

import Nope

import Nope.Tests.ParsingTests
import Nope.Tests.NameDeclarationTests
import Nope.Tests.NameResolutionTests
import Nope.Tests.TypeCheckerTests
import Nope.Tests.DesugaringTests
import Nope.Tests.CousCous.InterpreterTests
import Nope.Tests.ExecutionTests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
    parsingTestSuite,
    nameDeclarationTestSuite,
    nameResolutionTestSuite,
    typeCheckerTestSuite,
    desugaringTestSuite,
    interpreterTestSuite,
    executionTestSuite
    ]
