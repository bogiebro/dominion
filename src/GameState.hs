{-# LANGUAGE TemplateHaskell, DataKinds, ParallelListComp,
  OverloadedLists, OverloadedStrings #-}
module GameState where
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Dist
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Vector (Vector)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Monoid

-- We need to have a gamestate for each opponent
-- And use the strongest opponent's state as the true opponent.

-- Game state
data GameState = GameState {
  _deck :: Dist,
  _opDeck :: Dist,
  _remaining :: U.Vector CardId,
  _needEliminated :: Int,
  _money :: Int,
  _buys :: Int,
  _actions :: Int,
  _hand :: [Card],
  _myTurn :: Bool
}

-- State affecting how we transition GameState
data PlayState = PlayState {
  _updateWeights :: Bool, 
  _simulated :: Bool
}

-- Gameplay component
type GameAction = ReaderT PlayState ((WriterT (First Text) (MaybeT IO)))

-- Gameplay component, in a particular running context
type PlayAction = StateT GameState GameAction

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
  defense :: Bool,
  initAmt :: Int
}

makeLenses ''GameState
makeLenses ''PlayState

-- Default card constructor
card :: Text -> Int -> Card
card n c = Card n (return ()) (const 0) c 0 Action False 10

-- Which moves are legal for a given State?
legalMoves :: GameState -> [PlayAction ()]
legalMoves = undefined

-- Should I simulate the PlayAction behavior?
simulate :: PlayAction ()
simulate = ((||) <$> use myTurn <*> view simulated) >>= guard

-- Play out a turn for the program
turn :: PlayAction ()
turn = checkDone >> ((simulate >> simAction >> simBuy) `mplus` liveAction >> liveBuy) where
  liveAction = do
    t <- liftIO (T.putStrLn "Play a card:" >> T.getLine)
    when (not (T.null t)) $ liftIO (parseCard t) >>= \(c, i)->
      opDeck %= discard i >> action c >> liveAction
  simAction = undefined

  liveBuy = do
    t <- liftIO (T.putStrLn "Buy a card:" >> T.getLine)
    when (not (T.null t)) $ liftIO (parseCard t) >>= \(c, i)->
      opDeck %= addDiscard i >> liveBuy
  simBuy = undefined

-- Play out a game, starting with the given cards available
game :: U.Vector CardId -> Int -> GameAction Int
game startDist players = score <$> execStateT gameMonad ini where
  gameMonad = forever $ do
    myTurn .= True >> turn
    myTurn .= False >> replicateM_ (players - 1) turn
  startDeck = mkDist [(snd $ cardNames M.! "Copper", 7), (snd $ cardNames M.! "Estate", 3)]
  ini = GameState {
    _deck = startDeck,
    _opDeck = startDeck,
    _remaining = startDist,
    _needEliminated = 3,
    _money = 0,
    _buys = 0,
    _actions = 0,
    _hand = [],
    _myTurn = True
  }

-- Check if a game is finished
checkDone :: PlayAction ()
checkDone = do
  allGone <- uses needEliminated (0 ==)
  let provinceIdx = fromIntegral (snd (cardNames M.! "Province"))
  provincesGone <- uses remaining ((0 ==) . (U.! provinceIdx))
  guard (allGone || provincesGone)

-- Score a completed game
score :: GameState -> Int
score = undefined
{-
score g = scoreCards (_deck g <> _discards g) - scoreCards (_opDeck g <> _opDiscards g) where
  scoreCards :: Dist -> Int
  scoreCards c = getSum $ distSum (Sum . flip points c . (standardDeck V.!)) c
-}

-- Move `i` cards from deck to hand
plusCard :: Int -> PlayAction ()
plusCard i = replicateM_ i
  (use deck >>= liftIO . draw >>= \(i, d)-> deck .= d >> hand %= (idToCard i:))

-- Look up a card by id
idToCard :: CardId -> Card
idToCard i = standardDeck V.! fromIntegral i

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

-- Index of deck by card name
cardNames :: Map Text (Card, CardId)
cardNames = M.fromList [(name x, (x, i)) | x <- V.toList standardDeck | i <- [0..]] 

-- Find a card with the given name, prompting the user to re-enter the name
-- if there's a problem.
parseCard :: Text -> IO (Card, CardId)
parseCard t = maybe err return (M.lookup t cardNames) where
  err = T.putStrLn "Invalid card. Try again:" >> T.getLine >>= parseCard

-- Get a y/n value from the user
getDecision :: MonadIO m => Text -> m Bool
getDecision t = (== "y") <$> (liftIO $ T.putStrLn (t <> ": (y/n)") >> T.getLine)
