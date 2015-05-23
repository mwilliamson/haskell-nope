module Nope.Results where

type Result a = Either NopeError a

data NopeError = Error
    deriving (Show, Eq)
