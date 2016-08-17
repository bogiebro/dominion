module Main where
import Options.Applicative
import Data.Traversable
import Data.Vector as V
import qualified Data.Text as T
import GameState
import MonteCarlo
import Data.Word

data Opts = Opts FilePath Int Bool (Vector Word8)

main :: IO ()
main = execParser desc >>= handle where
  desc = info (helper <*> parser) $ fullDesc
           <> progDesc "Play Dominion"
           <> header "dominion- because chess is too easy"
  parser = Opts <$> strOption (long "filename" <> short 'o'
                    <> help "File to load/save training data")
               <*> option auto (long "players" <> short 'p')
               <*> switch (long "train" <> short 't'
                    <> help "Whether to train the network")
               <*> parseCards

parseCards :: Parser (Vector Word8)
parseCards =
  let cs = sequenceA $ fmap (switch . long . T.unpack . name) (unsafeDrop 6 standardDeck)
      useCard b c = if b then initAmt c else 0
  in V.zipWith useCard <$> fmap (V.replicate 6 True <>) cs <*> pure standardDeck

handle :: Opts -> IO ()
handle (Opts trainPath players train cards) = undefined
