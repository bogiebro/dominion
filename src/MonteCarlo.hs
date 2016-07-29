module MonteCarlo (monteCarlo) where
import GameState
import Data.IORef
import System.Posix.Signals
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Concurrent

-- Play random games until the user says to stop; report the best move
monteCarlo :: Int -> GameState -> GameAction Move
monteCarlo threads g = maxMove <$> forM (legalMoves g) $ \(Move n a)-> do
 v <- liftIO $ newIORef startVal
 tids <- replicateM threads $ forever $ do
   mapReaderT (lift . forkIO . (>>= appendIt v) . runMaybeT) $ execStateT (a >> game) g
 awaitSignal (Just $ addSignal sigINT emptySignalSet)
 mapM_ killThread tids
 toAvg <$> readIORef v

startVal :: (Move, Int, Int)
startVal = undefined

appendIt :: IORef (Move, Int, Int) -> Int -> IO ()
appendIt = undefined

toAvg = undefined

maxMove = undefined

-- not execStateT. the ther one
