module Dist (Dist(..), mkDist, choose, addDist, removeDist, nullDist, getDist, distSum) where
import Control.Monad.Random

-- http://hackage.haskell.org/package/Dist could be an option
-- But it has logn sample times. I want O(1) amortised. 
--
-- http://hackage.haskell.org/package/distribution is another option.
-- It samples in constant time. Can it add/remove in constant time too?

-- obviously we'll need a real type
data Dist = Dist ()

instance Monoid Dist where
  mempty = Dist ()
  mappend _ _ = Dist ()

-- Like foldMap, but momomorphic
distSum :: Monoid m => (Int -> m) -> Dist -> m 
distSum = undefined

-- Make a distribution, given a list matching value to frequency
mkDist :: [(Int, Int)] -> Dist
mkDist = undefined

-- Empty distribution
nullDist :: Dist
nullDist = undefined

-- Draw a value from this distribution. O(1)
choose :: Dist -> Rand StdGen (Int, Dist)
choose = undefined

-- Remove a value from this distribution. O(1)
removeDist :: Int -> Dist -> Dist
removeDist = undefined

-- Add a value to this distribution. O(1)
addDist :: Int -> Dist -> Dist
addDist = undefined

-- Get the weight associated with this distribution
getDist :: Int -> Dist -> Int
getDist = undefined
