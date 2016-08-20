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

-- THIS SHOULD USE THE MAYBET
-- TO RETURN NOTHING WHEN NO MOVES ARE POSSIBLE/
-- GAME OVER.

-- Play random games until the user says to stop; report the best move
monteCarlo :: Int -> Weights -> GameState -> GameAction Move
monteCarlo threads w g = fmap maxMove $ forM (legalMoves g) $ \m@(Move n a)-> do
 v <- liftIO $ newIORef (0, 0)
 let spawn = mapReaderT (liftIO . forkIO . (>>= appendIt v) . runMaybeT) $
               flip evalStateT w $ evalStateT (a >> game) g 
 tids <- replicateM threads $ forever (spawn :: GameAction ThreadId)
 liftIO $ do
   awaitSignal (Just $ addSignal sigINT emptySignalSet)
   mapM_ killThread tids
   toAvg m <$> readIORef v

-- Update the counter and score sum associated with a move
appendIt :: IORef (Int, Double) -> Maybe Double -> IO ()
appendIt _ Nothing = return ()
appendIt ior (Just upd) =
  atomicModifyIORef' ior (\(total, score)-> ((total+1, score+upd), ()))

-- Calculate the average score per move
toAvg :: Move -> (Int, Double) -> (Move, Double)
toAvg m (total, score) = (m, score / fromIntegral total)

-- Get the move with the best average score
maxMove :: [(Move, Double)] -> Move
maxMove = fst . maximumBy (comparing snd)

