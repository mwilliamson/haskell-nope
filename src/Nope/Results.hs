module Nope.Results where

import Control.Monad

data Result a = Success a


instance Monad Result where
    return = Success
    
    (Success x) >>= f = f x

instance Functor Result where
    fmap = liftM

instance Applicative Result where
    pure = return
    (<*>) = ap
