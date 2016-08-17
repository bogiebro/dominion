module MonteCarlo (monteCarlo) where
import GameState
import Data.IORef
import System.Posix.Signals
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Concurrent
import Data.List (maximumBy)
import Data.Ord (comparing)

-- Play random games until the user says to stop; report the best move
monteCarlo :: Int -> GameState -> GameAction Move
monteCarlo threads g = fmap maxMove $ forM (legalMoves g) $ \m@(Move n a)-> do
 v <- liftIO $ newIORef (0, 0)
 let spawn = mapReaderT (liftIO . forkIO . (>>= appendIt v) . runMaybeT) $ evalStateT (a >> game) g 
 tids <- replicateM threads $ forever (spawn :: GameAction ThreadId)
 liftIO $ do
   awaitSignal (Just $ addSignal sigINT emptySignalSet)
   mapM_ killThread tids
   toAvg m <$> readIORef v

-- Update the counter and score sum associated with a move
appendIt :: IORef (Int, Int) -> Maybe Int -> IO ()
appendIt _ Nothing = return ()
appendIt ior (Just upd) =
  atomicModifyIORef' ior (\(total, score)-> ((total+1, score+upd), ()))

-- Calculate the average score per move
toAvg :: Move -> (Int, Int) -> (Move, Double)
toAvg m (total, score) = (m, fromIntegral score / fromIntegral total)

-- Get the move with the best average score
maxMove :: [(Move, Double)] -> Move
maxMove = fst . maximumBy (comparing snd)

