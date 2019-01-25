module Aeson.ValueParser.Vector
where

import Aeson.ValueParser.Prelude hiding (Vector)
import Data.Vector.Generic
import qualified Data.Vector.Fusion.Bundle.Monadic as BundleM


{-# INLINE ifoldrM #-}
ifoldrM :: (Vector v a, Monad m) => (Int -> a -> b -> m b) -> b -> v a -> m b
ifoldrM f z = BundleM.foldrM (uncurry f) z . BundleM.indexed . BundleM.fromVector
