module Nope.Sources where

data Source = Source SourceDescription String

data SourceDescription = AnonymousSource
    deriving (Show, Eq)

data SourceLocation =
    SourcePoint SourceDescription Int Int |
    SourceFile SourceDescription
        deriving (Show, Eq)
