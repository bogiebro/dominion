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

-- repeat infinitely until sigINT
-- pick 12 cards to make your (Vector Word8)
-- play a full game

handle :: Opts -> IO ()
handle (Opts trainPath players threads t True _) = undefined
handle (Opts trainPath players threads t False cards) =
  let ?threads = threads
      ?myTurn = t
      ?updateWeights = False
      ?simulated = False
      ?turn = 0
  in do
    w <- readWeights trainPath
    _ <- runMaybeT $ execStateT (execStateT game (startState cards players)) w
    T.putStrLn "Game Over"
