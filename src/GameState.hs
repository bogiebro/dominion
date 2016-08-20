{-# LANGUAGE ImplicitParams, StrictData #-} 
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
import Data.MemoTrie
import Data.Word
import Data.List
import Data.Ord
import System.Random
import System.Posix.Signals
import Control.Concurrent
import Data.IORef


-- WE SHOULD probably translate this to use reification
-- instead of implicit parameters

-- Info for a single player
data PlayerState = PlayerState {
  _deck :: Dist,
  _score :: Int,
  _deckMul :: Int
}

-- Game state
data GameState = GameState {
  _players :: Vector PlayerState, -- each player's deck and points
  _remaining :: U.Vector Word8, -- cards left in inventory
  _needEliminated :: Word8, -- number of piles to go before game end
  _money :: Word8, -- amt of money remaining
  _buys :: Word8, -- number of buys remaining
  _actions :: Word8, -- number of actions remaining
  _hand :: [Card] -- only includes action cards
}

-- Learned data
type Weights = ()

-- Gameplay component
type GameAction = MaybeT IO

-- Gameplay component, in a particular running context
type PlayAction = StateT GameState (StateT Weights GameAction)

-- Dynamically scoped configuration variables
type Config = (?turn :: Int, ?simulated :: Bool, ?myTurn :: Int,
                ?updateWeights :: Bool, ?threads :: Int)

-- A move is a PlayAction, with a name for reporting it to the client
-- It can either be playing a card, buying a card
data Move = Move {
  moveName :: Text,
  moveAction :: PlayAction ()
}

-- Categories of cards, often for determining what's legal to draw
data CardType = Victory | Treasure | Action deriving (Eq)

-- Properties of a card, regardless of CardType
data Card = Card {
  name :: Text,
  action :: Config => PlayAction (),
  points :: Int,
  cost :: Word8,
  more :: Word8,
  cardType :: CardType,
  defense :: Bool,
  initAmt :: Word8
}

makeLenses ''PlayerState
makeLenses ''GameState

-- Default card constructor
card :: Text -> Word8 -> Card
card n c = Card n (return ()) 0 c 0 Action False 10

-- Lens into the current player's state
player :: (?turn :: Int) => Lens' GameState PlayerState
player = players.ix(?turn)

-- Which moves are legal for a given State?
legalMoves :: Config => GameState -> [Move]
legalMoves g =
  if _actions g > 0 then Move <$> (("Play " <>) . name) <*> action <$> _hand g
  else buyCards <$> findBuys (_buys g) (_money g) 0

-- Find all possible purchase sets
findBuys :: Word8 -> Word8 -> CardId -> [[CardId]]
findBuys = memo3 findBuys' where
  findBuys' 0 money i = []
  findBuys' buys money i
    | not (validCardId i) = []
    | ccost i < money =
      (map (i:) $ findBuys (buys - 1) (money - ccost i) (i+1)) ++ findBuys buys money (i+1)
    | otherwise = findBuys buys money (i+1)
  ccost i = cost (idToCard i)

-- Create a Move of buying a set of cards
buyCards :: Config => [CardId] -> Move
buyCards cis = Move
  ("Buy " <> T.intercalate ", " (map (name . idToCard) cis))
  (mapM_ addMyDiscard cis >> buys .= 0 >> money .= 0)

-- Sample a list of values and their probabilities
sample :: [a] -> (a -> Double) -> IO a
sample [x] _ = return x
sample a@(x:xs) f = do
  let s = sum (map f a)
      cs = scanl (\(_,q) y-> (y, q + f y)) (x, f x)  xs
  p <- randomRIO (0.0,s)
  return . fst . head $ dropWhile (\(_,q) -> q < p) cs

-- Get possible moves, result states, and weights
weightedMoves :: Config => PlayAction [(Move, GameState, Double)]
weightedMoves = do
  possible <- gets legalMoves
  s <- get
  w <- lift get
  let ?simulate = True
  states <- lift $ mapM (flip execStateT s . moveAction) possible
  return $ zip3 possible states (map (expectedScore w) states)

-- Check if a game is finished
notDone :: PlayAction Bool
notDone = do
  allGone <- uses needEliminated (0 ==)
  let provinceIdx = fromIntegral (snd (cardNames M.! "province"))
  provincesGone <- uses remaining ((0 ==) . (U.! provinceIdx))
  return (not (allGone || provincesGone))

-- Play all moves that do not decrease the number of available actions
playNoCostMoves :: Config => PlayAction ()
playNoCostMoves = do
  (nocost, others) <- uses hand (partition ((> 0) . more))
  case nocost of
    [] -> return ()
    xs -> mapM_ action xs >> hand .= others >> playNoCostMoves

-- Play out a turn for the program
playTurn :: Config => PlayAction ()
playTurn =
  notDone >>= guard >> if askUser then liveAction >> liveBuy else simMove where
    liveAction = do
      t <- liftIO (T.putStrLn "Play a card:" >> T.getLine)
      when (not (T.null t)) $ do
        (c, i) <- liftIO (parseCard t)
        player.deck %= discard i >> action c >> liveAction
    liveBuy = do
      t <- liftIO (T.putStrLn "Buy a card:" >> T.getLine)
      when (not (T.null t)) $ do
        (_, i) <- liftIO (parseCard t)
        player.deck %= addDiscard i >> liveBuy
    simMove = playNoCostMoves >> makeMove >> simMove

-- Should we ask the user how to make this move?
askUser :: Config => Bool
askUser = ?turn == ?myTurn || ?simulated

-- Play a move (playing or buying a card)
makeMove :: Config => PlayAction ()
makeMove = if ?simulated then sampleMove else monteCarlo

-- Sample a move according to weight and play it
sampleMove :: Config => PlayAction ()
sampleMove = do
  ms <- weightedMoves
  (m, st', _) <- liftIO $ sample ms (^._3)
  when ?updateWeights $ do
    nd <- notDone
    (_, maxQ) <- lift $ fmap (maximumBy (comparing snd)) $ evalStateT weightedMoves st'
    train (if nd then maxQ else getScore st')
  doMove m 

-- Execute the action encoded in a Move
doMove :: Config => Move -> PlayAction ()
doMove m = do
  when (not ?simulated) $ T.putStrLn (moveName m)
  moveAction m

-- Play random games until the user says to stop; play the best move
monteCarlo :: Config => PlayAction ()
monteCarlo = do
  ms <- gets legalMoves
  join . fmap maxMove . forM ms $ \m@(Move n a)-> do
    v <- liftIO $ newIORef (0, 0)
    tids <- replicateM ?threads (forever (a >> game >>= liftIO . appendIt v))
    liftIO $ do
      awaitSignal (Just $ addSignal sigINT emptySignalSet)
      mapM_ killThread tids
      toAvg m <$> readIORef v

-- Update the counter and score sum associated with a move
appendIt :: IORef (Int, Double) -> Double -> IO ()
appendIt ior upd =
  atomicModifyIORef' ior (\(total, score)-> ((total+1, score+upd), ()))

-- Calculate the average score per move
toAvg :: Move -> (Int, Double) -> (Move, Double)
toAvg m (total, score) = (m, score / fromIntegral total)

-- Run the move with the best average score
maxMove :: Config => [(Move, Double)] -> PlayAction ()
maxMove = doMove . fst . maximumBy (comparing snd)

-- Train the network based on the given y value
train :: Double -> PlayAction ()
train = undefined

-- Reinforcement learning policy
expectedScore :: Weights -> GameState -> Double
expectedScore = undefined

-- Play out a game, starting with the given cards available
game :: Config => PlayAction Double
game = do
  pl <- uses players V.length
  forM (cycle [0..pl]) $ \t-> do
    let ?turn = t
    plusCard 5
    actions .= 1
    buys .= 1
    playTurn
  gets getScore 

-- Starting game state
startState :: U.Vector Word8 -> Int -> GameState
startState startDist pl = ini where
  startDeck = mkDist [(snd $ cardNames M.! "copper", 7), (snd $ cardNames M.! "estate", 3)]
  ini = GameState {
    _players = V.replicate pl (PlayerState startDeck 0 1),
    _remaining = startDist,
    _needEliminated = 3,
    _money = 0,
    _buys = 0,
    _actions = 0,
    _hand = []
  }

-- Score a game
getScore :: Config => GameState -> Double
getScore g = fromIntegral (myScore - theirScore) where
  myScore = g ^?! player.score
  theirScore = getMax (g ^. players.to(zeroMe).each.score.to Max)
  zeroMe = ix(?myTurn).score .~ 0

-- Put a card into your hand
handleDraw :: Config => Card -> PlayAction ()
handleDraw c = case cardType c of
  Action -> hand %= (c:)
  Victory -> return ()
  Treasure -> action c

-- Add a card to your hand
addHand :: Config => CardId -> PlayAction ()
addHand c = addMyDiscard c >> handleDraw (idToCard c)

-- Unsafe version of use
use' :: MonadState s m => Getting (Endo a) s a -> m a
use' f = gets (^?! f)

-- Move `i` cards from deck to hand, discarding victories and applying treasure
plusCard :: Config => Int -> PlayAction ()
plusCard i =
  replicateM_ i $ (use' (player.deck) >>= liftIO . draw >>=
    \(i, d')-> player.deck .= d' >> handleDraw (idToCard i))

-- Add a card to the current player's discard pile
addMyDiscard :: Config => CardId -> PlayAction ()
addMyDiscard i = player.deck %= addDiscard i

-- Lookup a card by id
idToCard :: CardId -> Card
idToCard i = standardDeck V.! fromIntegral i

-- Does this Word8 represent a valid card id?
validCardId :: CardId -> Bool
validCardId i = fromIntegral i < V.length standardDeck

-- Base dominion deck (no expansions)
standardDeck :: Vector Card
standardDeck = [
  (card "copper" 0) {action = money += 1, cardType = Treasure},
  (card "silver" 3) {action = money += 2, cardType = Treasure},
  (card "gold" 6) {action = money += 3, cardType = Treasure},
  (card "estate" 2) {points = 1, cardType = Victory},
  (card "duchy" 5) {points = 3, cardType = Victory},
  (card "province" 8) {points = 6, cardType = Victory},
  (card "market" 2) {more = 1, action = do {plusCard 1; buys += 1; money += 1}},
  (card "laboratory" 4) {more = 1, action = plusCard 2},
  (card "militia" 4) {action = money += 2},
  (card "moat" 2) {action = plusCard 2, defense=True},
  (card "smithy" 3) {action = plusCard 3},
  (card "bureacrat" 4) {action = addHand (snd (cardNames M.! "silver"))},
  (card "village" 5) {more = 1, action = plusCard 1}]

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
