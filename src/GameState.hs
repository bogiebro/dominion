{-# LANGUAGE TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving,
    RankNTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances #-}
module GameState where
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
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

-- Environment in which to play the game
class Monad m => GamePlayer m where
  drawCard :: m Card
  playOpponent :: m ()

-- Play environment which draws cards by sampling a payoff function
newtype SimT m a = SimT (m a) deriving (Functor, Applicative, Monad)

instance MonadTrans SimT where lift = SimT

instance (Monad m, MonadRandom m, Learner m) => GamePlayer (SimT m) where
  drawCard = undefined
  playOpponent = undefined

instance MonadState s m => MonadState s (SimT m) where
    get = lift get
    put = lift . put
    state = lift . state

-- Play enviroment which draws cards by asking the user
newtype InteractT m a = InteractT (m a) deriving (Functor, Applicative, Monad)

instance MonadTrans InteractT where lift = InteractT

instance MonadIO m => GamePlayer (InteractT m) where
  drawCard = undefined
  playOpponent = undefined

instance MonadState s m => MonadState s (InteractT m) where
    get = lift get
    put = lift . put
    state = lift . state

-- Gameplay component
type PlayAction a = forall m. (GamePlayer m, MonadState GameState m) => m a

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
legalMoves :: MonadState GameState m => GameState -> [m ()]
legalMoves = undefined

-- Play out a turn for the program
turn :: PlayAction ()
turn = undefined

-- Play out a game by random simulation, starting with the given cards available
game :: (Learner (t (Rand StdGen))) => Dist -> SimT (t (Rand StdGen)) ()
game = undefined

-- Move `i` cards from deck to hand
plusCard :: Int -> PlayAction ()
plusCard i = replicateM_ i (drawCard >>= \d-> hand %= (d:))
