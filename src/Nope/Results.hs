module Nope.Results where

import Nope.Sources

type Result a = Either NopeError a

data NopeError = SyntaxError SourceLocation String
    deriving (Show, Eq)
