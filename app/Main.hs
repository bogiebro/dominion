{-# LANGUAGE ImplicitParams #-} 
module Main where
import Options.Applicative
import Data.Traversable
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GameState
import Control.Monad.Trans.Maybe
import Control.Monad.State.Strict
import Data.Word
import Dist
import qualified Data.IntMap as I
import Data.IntMap (IntMap)
import Control.Lens
import System.Random

data Opts = Opts FilePath Int Int Int Bool (U.Vector Word8)

main :: IO ()
main = execParser desc >>= handle where
  desc = info (helper <*> parser) $ fullDesc
           <> progDesc "Play Dominion"
           <> header "dominion- because chess is too easy"
  parser = Opts <$> strOption (long "filename" <> short 'o'
                    <> help "File to load/save training data")
               <*> option auto (long "players" <> short 'p' <> value 2)
               <*> option auto (long "threads" <> short 'n' <> value 1)
               <*> option auto (long "aiturn" <> short 't' <> value 0
                    <> help "When does the AI play?")
               <*> switch (long "train" <> short 'l'
                    <> help "Whether to train the network")
               <*> parseCards

parseCards :: Parser (U.Vector Word8)
parseCards =
  let std = V.toList standardDeck
      cs = sequenceA $ fmap (switch . long . T.unpack . name) (drop 6 std)
      useCard b c = if b then initAmt c else 0
  in fmap U.fromList $ zipWith useCard <$> fmap (replicate 6 True <>) cs <*> pure std

readWeights :: FilePath -> IO Weights
readWeights = undefined

-- Pick 12 random card classes to start the game
dealIt :: IO [CardId]
dealIt = fmap (snd . mapAccumL f I.empty) rs where
  rs = mapM (\l-> (l,) <$> randomRIO (0,l)) [lastCard .. lastCard - 12]
  f :: IntMap Int -> (Int, Int) -> (IntMap Int, CardId)
  f a (l, i) = (a & at(i) ?~ maybe l id (a^?ix(l)),
                maybe (fromIntegral i) fromIntegral (a^?ix(i)))
  lastCard = V.length standardDeck - 1

randState :: IO (U.Vector Word8)
randState = foldl (\v i-> v & ix(fromIntegral i) .~ initAmt (idToCard i))
  (U.replicate (V.length standardDeck) 0) <$> dealIt

handle :: Opts -> IO ()
handle (Opts trainPath players threads t sim cards) = do
  w <- readWeights trainPath
  let ?threads = threads
      ?myTurn = t
      ?updateWeights = False
      ?simulated = sim
      ?turn = 0
  st <- if sim then startState players <$> randState else return (startState players cards)
  let m = runMaybeT (execStateT (execStateT game st) w) >> return ()
  if sim then workers m else m
