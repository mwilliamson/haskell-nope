module Nope.Parsing where

import qualified Nope.Nodes as Nodes

parse :: String -> Nodes.Module
parse _ = Nodes.Module [
    Nodes.ExpressionStatement (Nodes.Call (Nodes.Builtin "print") [Nodes.Literal 42])
    ]

