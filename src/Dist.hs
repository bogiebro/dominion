{-# LANGUAGE TemplateHaskell #-}
module Dist (
  Dist(..), mkDist, draw, addDiscard, discard, 
  removeTop, nullDist, getDist, CardId) where
import System.Random
import Data.Vector.Unboxed as U
import Data.Word
import Control.Lens
import Data.List as L

-- Each class of card has an id
type CardId = Word8

-- A probability distribution on cards, representing both deck and discard pile
data Dist = Dist {
  _sample :: Vector CardId, -- set of cards present in the current deck
  _rep :: Vector CardId,    -- tracks multiplicity of each card in the current deck
  _fresh :: Vector CardId,  -- tracks multiplicity of each card in a shuffled deck
  _discStart :: Int,        -- offset in 'sample' of discard pile's start
  _discEnd :: Int          -- offset in 'sample' of discard pile's end
}

makeLenses ''Dist

-- Constant for number of types of cards 
cardClasses :: Int
cardClasses = 25

-- Constant for total number of cards that can fit in a deck
totalCards :: Int
totalCards = 200

deckSize :: Dist -> Int
deckSize d = d^.discEnd

-- Empty distribution on c card classes, where the total total number of cards cannot exceed n
nullDist :: Dist
nullDist = Dist (U.replicate totalCards 0)
  (U.replicate cardClasses 0) (U.replicate cardClasses 0) 0 0

-- Add a card to the discard pile
addDiscard :: CardId -> Dist -> Dist
addDiscard i d = d & fresh . ix (fromIntegral i) +~ 1
                   & sample . ix (d ^. discEnd) .~ i
                   & discEnd +~ 1

-- Make  distribution in the discard pile
mkDist :: [(CardId, Int)] -> Dist
mkDist = L.foldl (\d (c, i)-> iterate (addDiscard c) d !! i) nullDist

-- Shuffle a distribution, placing the discard pile back in the deck
shuffle :: Dist -> Dist
shuffle d = d & discStart .~ (d^.discEnd)
              & rep .~ (d ^. fresh)

-- Draw a card from this distribution.
draw :: Dist -> IO (CardId, Dist)
draw d = do
  r <- randomRIO (0, d^.discStart)
  return $ discardIdx d r

-- Discard a the card at a given index
discardIdx :: Dist -> Int -> (CardId, Dist)
discardIdx d r = (i, d') where
  i = d^?!sample.ix(r)
  dst = d^.discStart - 1
  d' = d & sample.ix(dst) .~ i & sample.ix(r) .~ (d^?!sample.ix(dst))
         & discStart -~ 1

-- Move this card from deck to discard. Takes O(n) time.
discard :: CardId -> Dist  -> Dist
discard i d = d' where
  (Just j) = U.elemIndex i (d ^. sample)
  (_, d') = discardIdx d j

-- Remove the top of the discard pile, useful after drawing with 'draw'.
removeTop :: Dist -> Dist
removeTop d = d & sample.ix(d^.discStart) .~ (d^?!sample.ix(d^.discEnd - 1))
                & discEnd -~ 1

-- Get the weight associated with this distribution
getDist :: Int -> Dist -> Word8
getDist i d = d^?!rep.ix(i)
