module Nope.Tests.Results where

import Test.Tasty.HUnit

import Nope
import Nope.Results


assertResult :: Result Stdout -> Result Stdout -> Assertion
assertResult (Right (Stdout expectedStdout)) (Right (Stdout actualStdout)) =
    expectedStdout @=? actualStdout

