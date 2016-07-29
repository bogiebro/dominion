{-# LANGUAGE TemplateHaskell, ParallelListComp, OverloadedLists, OverloadedStrings #-}
module GameState where
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Reader
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
import Data.Semigroup (Max(..))

-- Info for a single player
data PlayerState = PlayerState {
  _deck :: Dist,
  _score :: Int,
  _deckMul :: Int
}

-- Game state
data GameState = GameState {
  _players :: Vector PlayerState,
  _remaining :: U.Vector CardId,
  _needEliminated :: Int,
  _money :: Int,
  _buys :: Int,
  _actions :: Int,
  _hand :: [Card],
  _turn :: Int
}

-- State affecting how we transition GameState
data PlayState = PlayState {
  _updateWeights :: Bool, 
  _simulated :: Bool
}

-- Gameplay component
type GameAction = ReaderT PlayState (MaybeT IO)

-- Gameplay component, in a particular running context
type PlayAction = StateT GameState GameAction

-- A move is a PlayAction, with a name for reporting it to the client
data Move = Move {
  moveName :: Text,
  moveAction :: PlayAction ()
}
instance Eq Move where (Move a _) == (Move b _) = a == b
instance Ord Move where compare (Move a _) (Move b _) = compare a b

-- Categories of cards, often for determining what's legal to draw
data CardType = Victory | Treasure | Action deriving (Eq)

-- Properties of a card, regardless of CardType
data Card = Card {
  name :: Text,
  action :: PlayAction (),
  points :: Int,
  cost :: Int,
  more :: Int, 
  cardType :: CardType,
  defense :: Bool,
  initAmt :: Int
}

makeLenses ''PlayerState
makeLenses ''GameState
makeLenses ''PlayState

-- Default card constructor
card :: Text -> Int -> Card
card n c = Card n (return ()) 0 c 0 Action False 10

-- Which moves are legal for a given State?
legalMoves :: GameState -> [Move]
legalMoves g =
  if _actions g > 0 then Move <$> (("Play " <>) . name) <*> action <$> _hand g
  else undefined 

-- Should I simulate the PlayAction behavior?
simulate :: PlayAction ()
simulate = ((||) <$> uses turn (==0) <*> view simulated) >>= guard

-- Choose the best playAction of the list to play, or mzero if null
bestAction :: [PlayAction ()] -> PlayAction ()
bestAction = undefined

-- Play out a turn for the program
playTurn :: PlayAction ()
playTurn = checkDone >> ((simulate >> simMove) `mplus` (liveAction >> liveBuy)) where
  liveAction = do
    t <- liftIO (T.putStrLn "Play a card:" >> T.getLine)
    turnIdx <- use turn
    when (not (T.null t)) $ do
      (c, i) <- liftIO (parseCard t)
      players.ix(turnIdx).deck %= discard i >> action c >> liveAction
  liveBuy = do
    t <- liftIO (T.putStrLn "Buy a card:" >> T.getLine)
    turnIdx <- use turn
    when (not (T.null t)) $ do
      (_, i) <- liftIO (parseCard t)
      players.ix(turnIdx).deck %= addDiscard i >> liveBuy
  simMove = gets (fmap moveAction . legalMoves) >>= bestAction >> simMove

-- Play out a game, starting with the given cards available
game :: PlayAction Int
game = do
  pl <- uses players V.length 
  forM (cycle [0..pl]) $ \t-> turn .= t >> playTurn
  gets getScore 

-- Starting game state
startState :: U.Vector CardId -> Int -> GameState
startState startDist pl = ini where
  startDeck = mkDist [(snd $ cardNames M.! "Copper", 7), (snd $ cardNames M.! "Estate", 3)]
  ini = GameState {
    _players = V.replicate pl (PlayerState startDeck 0 1),
    _remaining = startDist,
    _needEliminated = 3,
    _money = 0,
    _buys = 0,
    _actions = 0,
    _hand = [],
    _turn = 0
  }

-- Score a game
getScore :: GameState -> Int
getScore g = myScore - theirScore where
  myScore = g ^?! players.ix(0).score
  theirScore = getMax (g ^. dropping 1 players.each.score.to Max)

-- Unsafe version of use
use' :: MonadState s m => Getting (Endo a) s a -> m a
use' f = gets (^?! f)

-- Check if a game is finished
checkDone :: PlayAction ()
checkDone = do
  allGone <- uses needEliminated (0 ==)
  let provinceIdx = fromIntegral (snd (cardNames M.! "Province"))
  provincesGone <- uses remaining ((0 ==) . (U.! provinceIdx))
  guard (allGone || provincesGone)

-- Move `i` cards from deck to hand
plusCard :: Int -> PlayAction ()
plusCard i = do
  tidx <- use turn
  replicateM_ i $ (use' (players.ix(tidx).deck) >>= liftIO . draw >>=
    \(i, d')-> players.ix(tidx).deck .= d' >> hand %= (idToCard i:))

-- Lookup a card by id
idToCard :: CardId -> Card
idToCard i = standardDeck V.! fromIntegral i

-- Base dominion deck (no expansions)
standardDeck :: Vector Card
standardDeck = [
  (card "Copper" 0) {action = money += 1, cardType = Treasure},
  (card "Silver" 3) {action = money += 2, cardType = Treasure},
  (card "Gold" 6) {action = money += 3, cardType = Treasure},
  (card "Estate" 2) {points = 1, cardType = Victory},
  (card "Duchy" 5) {points = 3, cardType = Victory},
  (card "Province" 8) {points = 6, cardType = Victory},
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
