module Nope.Desugaring where

import qualified Nope.Nodes as Nope
import qualified Nope.CousCous as CousCous

desugar :: Nope.Module -> CousCous.ModuleNode
desugar moduleNode = CousCous.ModuleNode [
    CousCous.expression_statement (CousCous.call (CousCous.builtin "print") [CousCous.literal 42])
    ]
