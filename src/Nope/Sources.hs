module Nope.Sources where

data Source = Source SourceDescription String

data SourceDescription = AnonymousSource
    deriving (Show, Eq)

data SourceLocation = SourceLocation SourceDescription Int Int
    deriving (Show, Eq)
