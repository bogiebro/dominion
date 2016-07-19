module Main where
import Options.Applicative
import Data.Traversable
import Data.Vector
import qualified Data.Text as T
import GameState

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

parseCards :: Parser (Vector Bool)
parseCards = sequenceA $ fmap (switch . long . T.unpack . name) standardDeck 

data Opts = Opts FilePath Int Bool (Vector Bool)

handle :: Opts -> IO ()
handle opts = undefined
