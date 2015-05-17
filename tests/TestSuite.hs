import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Nope

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
    programTestCase "Printing integer prints that number to stdout"
        "print(42)"
        (Result (Stdout "42\n"))
    ]


programTestCase :: [Char] -> [Char] -> Result -> TestTree
programTestCase testName programText expectedResult =
    testCase testName $
        let result = runProgram programText
        in assertResult expectedResult result


assertResult :: Result -> Result -> Assertion
assertResult (Result (Stdout expectedStdout)) (Result (Stdout actualStdout)) =
    expectedStdout @=? actualStdout
