{-# LANGUAGE TemplateHaskell, ExistentialQuantification, OverloadedStrings,
    RankNTypes, KindSignatures, TypeSynonymInstances, FlexibleInstances,
    MultiParamTypeClasses #-}
module GameState where
import Control.Lens
import Control.Monad.State
import Data.Text (Text)
import Control.Monad.Random

import Dist

-- Game state
data GameState = GameState {
  _deck :: Dist,
  _discards :: Dist,
  _remaining :: Dist,
  _needEliminated :: Int,
  _money :: Int,
  _buys :: Int,
  _actions :: Int,
  _hand :: [Card] 
} 

-- Monad that makes use of machine learning
class Monad m => Learner m where
  calcPayoff :: GameState -> m Int

-- A component of multi-turn gameplay, using training monad t and play monad m
type Player t (m :: * -> *) = StateT GameState (t m)

-- Players associated with cards; existentially qualified over Learner monad
data PlayAction a = forall t m. P (GamePlayer m, Learner (t m)) => PlayAction {
  applyAction :: Player t m a
}

-- Pretty sure I can get rid of these somehow
instance Monad PlayAction where
  return = PlayAction . return
  (PlayAction m) >>= f = PlayAction (m >>= f)

instance MonadState GameState PlayAction where
  get = PlayAction get
  put s = PlayAction $ put s
  state f = PlayAction $ state f
  

-- Environment in which to play the game
class GamePlayer (m :: * -> *) where
  drawCard :: Player t m Card
  playOpponent :: Learner (t m) => Player t m ()

-- Play environment which draws cards by sampling
instance GamePlayer (Rand StdGen) where
  drawCard = undefined
  playOpponent = undefined

-- Play environment which draws cards by asking the user
instance GamePlayer IO where
  drawCard = undefined
  playOpponent = undefined

-- Categories of cards, often for determining what's legal to draw
data CardType = Victory | Treasure | Action deriving (Eq)

-- Properties of a card, regardless of CardType
data Card = Card {
  name :: Text,
  action :: PlayAction (),
  points :: Dist -> Int,
  cost :: Int,
  more :: Int, 
  cardType :: CardType,
  defense :: Bool
}

makeLenses ''GameState

-- Default card constructor
card :: Text -> Int -> Card
card n c = Card n (return ()) (const 0) c 0 Action False

-- Treasure
copper, silver, gold :: Card
copper = (card "Copper" 0) {action = money += 1, cardType = Treasure}
silver = (card "Silver" 3) {action = money += 2, cardType = Treasure}
gold = (card "Gold" 6) {action = money += 3, cardType = Treasure}

-- Victory
estate, duchy, province :: Card
estate = (card "Estate" 2) {points = const 1, cardType = Victory}
duchy = (card "Duchy" 5) {points = const 3, cardType = Victory}
province = (card "Province" 8) {points = const 6, cardType = Victory}

-- Which moves are legal for a given State?
legalMoves :: GameState -> [PlayAction ()]
legalMoves = undefined

-- Play out a turn for the program
turn :: (GamePlayer m, Learner (t m)) => Player t m ()
turn = undefined

-- Play out a game by random simulation, starting with the given cards available
game :: Dist -> Player t (Rand StdGen) ()
game = undefined

-- Move `i` cards from deck to hand
plusCard :: Int -> PlayAction ()
plusCard i = PlayAction $ replicateM_ i (drawCard >>= \d-> hand %= (d:))
