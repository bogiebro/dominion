{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, OverloadedLists,
    RankNTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
    ParallelListComp, UndecidableInstances #-}
module GameState where
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Random
import Dist
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Map as M
import Data.Map (Map)

-- Note: these type signatures get really hairy. 
-- A lot of this would get simplified if I switched from using mtl
-- to using extendible-effects. Worth trying?

-- Game state
data GameState = GameState {
  _deck :: Dist,
  _discards :: Dist,
  _opDeck :: Dist,
  _opDiscards :: Dist,
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
class (Monad m, MonadState GameState m) => GamePlayer m where
  drawCard :: m Card
  harmOpponent :: m Bool
  playOpponent :: m ()

-- Gameplay component
type PlayAction a = forall m. (GamePlayer m) => m a

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

-- Which moves are legal for a given State?
legalMoves :: MonadState GameState m => GameState -> [m ()]
legalMoves = undefined

-- Play out a turn for the program
turn :: PlayAction ()
turn = undefined

-- Play out a game by random simulation, starting with the given cards available
game :: (Learner (t (Rand StdGen)), MonadState GameState (t (Rand StdGen)),
        MonadRandom (t (Rand StdGen))) => Dist -> Int -> SimT (t (Rand StdGen)) Int
game cardDist players = runMaybeT gameMonad >> gets score where
  gameMonad = forever $ checkDone >> lift turn >>
    replicateM_ (players - 1) (checkDone >> lift playOpponent)
  startDeck = mkDist [(snd $ cardNames M.! "Copper", 7/10), (snd $ cardNames M.! "Estate", 3/10)]
  initSt = GameState {
    _deck = startDeck,
    _discards = nullDist,
    _opDeck = startDeck,
    _opDiscards = nullDist,
    _remaining = cardDist,
    _needEliminated = 3,
    _money = 0,
    _buys = 0,
    _actions = 0,
    _hand = []
  }

-- Check if a game is finished
checkDone :: GamePlayer m => MaybeT m ()
checkDone = undefined

-- Score a completed game
score :: GameState -> Int
score = undefined

-- Move `i` cards from deck to hand
plusCard :: Int -> PlayAction ()
plusCard i = replicateM_ i (drawCard >>= \d-> hand %= (d:))

-- Base dominion deck (no expansions)
standardDeck :: Vector Card
standardDeck = [
  (card "Copper" 0) {action = money += 1, cardType = Treasure},
  (card "Silver" 3) {action = money += 2, cardType = Treasure},
  (card "Gold" 6) {action = money += 3, cardType = Treasure},
  (card "Estate" 2) {points = const 1, cardType = Victory},
  (card "Duchy" 5) {points = const 3, cardType = Victory},
  (card "Province" 8) {points = const 6, cardType = Victory},
  (card "Market" 2) {action = do {plusCard 1; buys += 1; money += 1}, more = 1},
  (card "Laboratory" 4) {action = plusCard 2, more = 1},
  (card "Militia" 4) {action = money += 2},
  (card "Moat" 2) {action = plusCard 2, defense=True},
  (card "Smithy" 3) {action = plusCard 3},
  (card "Bureacrat" 4) {action = hand %= (fst (cardNames M.! "Silver") :)},
  (card "Village" 5) {action = plusCard 1, more=1}]

-- Play environment which draws cards by sampling a payoff function
newtype SimT m a = SimT (m a) deriving (Functor, Applicative, Monad)
instance (Monad m, MonadState GameState m, MonadRandom m, Learner m) => GamePlayer (SimT m) where
  drawCard = undefined
  harmOpponent = undefined
  playOpponent = undefined

-- Boilerplate instances
instance MonadTrans SimT where lift = SimT
instance MonadState s m => MonadState s (SimT m) where
    get = lift get
    put = lift . put
    state = lift . state

-- Play enviroment which draws cards by asking the user
newtype InteractT m a = InteractT (m a) deriving (Functor, Applicative, Monad)
instance (MonadIO m, MonadState GameState m) => GamePlayer (InteractT m) where
  drawCard = liftIO $ T.putStrLn "Draw a card:" >> T.getLine >>= fmap fst . parseCard
  playOpponent = runInterpreter
  harmOpponent = (== "y") <$> (liftIO $ T.putStrLn "Use this action on opponent?:" >> T.getLine)

-- Boilerplate instances
instance MonadTrans InteractT where lift = InteractT
instance MonadIO m => MonadIO (InteractT m) where
    liftIO = InteractT . liftIO
instance MonadState s m => MonadState s (InteractT m) where
    get = lift get
    put = lift . put
    state = lift . state

-- Index of deck by card name
cardNames :: Map Text (Card, Int)
cardNames = M.fromList [(name x, (x, i)) | x <- V.toList standardDeck | i <- [0..]] 

-- Find a card with the given name, prompting the user to re-enter the name
-- if there's a problem.
parseCard :: Text -> IO (Card, Int)
parseCard t = maybe err return (M.lookup t cardNames) where
  err = T.putStrLn "Invalid card. Try again:" >> T.getLine >>= parseCard

-- Construct a PlayAction by asking the user what to do
runInterpreter :: (MonadIO m, MonadState GameState m) => InteractT m ()
runInterpreter = actionPhase >> buyPhase where
  actionPhase = liftIO (T.putStrLn "Play a card:" >> T.getLine) >>= \t ->
    if T.null t then return () else liftIO (parseCard t) >>= \(c, i)->
      opDiscards %= addDist i >> opDeck %= removeDist i >> action c >> actionPhase 
  buyPhase = liftIO (T.putStrLn "Buy a card:" >> T.getLine) >>= \t ->
    if T.null t then return () else liftIO (parseCard t) >>= \(c, i)->
      opDiscards %= addDist i >> buyPhase
