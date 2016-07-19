module MonteCarlo (monteCarlo) where
import Control.Monad.Random

import GameState

-- Play random games until the user says to stop; pick the best move
monteCarlo :: (Learner (t (Rand StdGen)), Learner (t IO)) =>
  SimT (t (Rand StdGen)) () -> InteractT (t IO) ()
monteCarlo = undefined
