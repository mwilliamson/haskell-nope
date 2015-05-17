module Nope.Tests.Results where

import Test.Tasty.HUnit

import Nope


assertResult :: Result -> Result -> Assertion
assertResult (Result (Stdout expectedStdout)) (Result (Stdout actualStdout)) =
    expectedStdout @=? actualStdout

