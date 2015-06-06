module Nope.CousCous.Interpreter.Heap (Heap(..), Address(..), empty, declare, assign, lookup) where

import Prelude hiding (lookup)
import Control.Monad (join)
import qualified Data.Map.Strict as Map

import qualified Nope.CousCous.Values as Values

newtype Address = Address Int deriving (Eq, Ord)
data Heap = Heap {
    heapVariables :: Map.Map Address (Maybe Values.Value),
    heapIndex :: Int
}

empty :: Heap
empty = Heap {
    heapVariables = Map.empty,
    heapIndex = 1
}

declare :: Maybe Values.Value -> Heap -> (Address, Heap)
declare value heap =
    let index = heapIndex heap
        address = Address index
        variables = Map.insert address value (heapVariables heap)
        heap' = heap { heapIndex = index + 1, heapVariables = variables }
    in (address, heap')

assign :: Address -> Values.Value -> Heap -> Heap
assign address value heap =
    heap { heapVariables = Map.insert address (Just value) (heapVariables heap) }

lookup :: Address -> Heap -> Maybe Values.Value
-- TODO: missing keys in the map are actually an error in the interpreter, so should probably be handled separately
lookup address heap = join $ Map.lookup address (heapVariables heap)
