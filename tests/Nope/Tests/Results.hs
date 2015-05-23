module Nope.Tests.Results where

import Test.Tasty.HUnit

import Nope
import Nope.Results


assertResult :: Result Stdout -> Result Stdout -> Assertion
assertResult (Success (Stdout expectedStdout)) (Success (Stdout actualStdout)) =
    expectedStdout @=? actualStdout

