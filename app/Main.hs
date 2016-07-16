module Main where
import Options.Applicative

main :: IO ()
main = execParser opts >>= handle where
  opts = info (helper <*> parser) $ fullDesc
           <> progDesc "Play Dominion"
           <> header "dominion- because chess is too easy"
  parser = (,) <$> strOption (long "filename" <> short 'o'
                    <> help "File to load/save training data")
               <*> switch (long "train" <> short 't'
                    <> help "Whether to train the network")


handle :: (FilePath, Bool) -> IO ()
handle (trainFile, train) = undefined
