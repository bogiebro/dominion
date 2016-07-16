module Dist (Dist(..), mkDist, choose, addDist, removeDist) where

-- obviously we'll need a real type
data Dist = Dist ()

-- Make a distribution, given a list matching value to frequency
mkDist :: [(Int, Int)] -> CardDist

-- Draw a value from this distribution. O(1)
choose :: Dist -> Rand StdGen (Int, Dist)
choose = undefined

-- Remove a value from this distribution. O(2
removeDist :: Int -> Dist -> Dist
removeDist = undefined

-- Add a value to this distribution. O(1)
addDist :: Int -> Dist -> Dist
addDist = undefined


