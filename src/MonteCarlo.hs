module MonteCarlo (monteCarlo) where
import GameState

-- Play random games until the user says to stop; pick the best move
monteCarlo :: GamePlayer (Rand StdGen) () -> GamePlayer IO ()
monteCarlo = undefined
