module Main where
import Options.Applicative
import Data.Traversable
import Data.Vector as V
import qualified Data.Text as T
import GameState
import MonteCarlo
import Data.Word

data Opts = Opts FilePath Int Int Bool (Vector Word8)

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

parseCards :: Parser (Vector Word8)
parseCards =
  let cs = sequenceA $ fmap (switch . long . T.unpack . name) (unsafeDrop 6 standardDeck)
      useCard b c = if b then initAmt c else 0
  in V.zipWith useCard <$> fmap (V.replicate 6 True <>) cs <*> pure standardDeck

handle :: Opts -> IO ()
handle (Opts trainPath players threads True _) =
  -- repeat infinitely until sigINT
  -- pick 12 cards to make your (Vector Word8)
  -- play a full game
 
handle (Opts trainPath players threads t False cards) = do
  w <- readWeights trainPath
  let ps = PlayState False True t
      playMove st = runMaybeT $ flip runReaderT ps $ monteCarlo threads w st >>= playMove
      playGame (Just st) = playMove st >>= playGame
      playGame Nothing = T.putStrLn "Game Over"
      playMove (Move t a) = liftIO (T.putStrLn t) >> a
  playGame (Just $ startState cards players)
-- WE CAN'T JUST USE THE NEW STATE
-- WE NEED TO RUN THE CHOSEN MOVE AGAIN, BUT WITH SIMULATED ON

-- MONTE CARLO NEEDS TO USE playTurn somwhere. 


-- when do we set simulated? also, we need to add Dist's freq stuff back, as
-- that's what gets sent to the neural network
