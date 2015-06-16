module Nope.Tests.Variables where

import Data.Char (ord)


declarationId name =
    let indices = enumFromTo 0 (length name)
    in sum $ zipWith (\character index -> (ord character) * 256 ^ index) name indices
