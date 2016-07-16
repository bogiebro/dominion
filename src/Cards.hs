{-# LANGUAGE OverloadedLists #-}
module Cards (standardDeck) where
import Control.Lens
import Data.Vector (Vector)

import GameState

-- Base dominion deck (no expansions)
standardDeck :: Vector Card
standardDeck = [
  copper, silver, gold, estate, duchy, province,

  card "Market" 2 {action = do {plusCard 1; buys += 1; money += 1}, more = 1},

  card "Laboratory" 4 {action = plusCard 2, more = 1},

  card "Militia" 4 {action = money += 2},

  card "Moat" 2 {action = plusCard 2; defense=True}

  card "Smithy" 3 {action = plusCard 3},

  card "Bureacrat" 4 {action = hand %= (silver:)},

  card "Village" 5 {action = plusCard 1, more=1}
]
