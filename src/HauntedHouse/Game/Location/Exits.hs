module HauntedHouse.Game.Location.Exits where

import Data.Map.Strict qualified (Map)
import HauntedHouse.Game.Labels (LocationLabel, ExitLabel)

newtype ExitMap = ExitMap 
  { _unExitMap :: Data.Map.Strict.Map ExitLabel LocationLabel}
   deriving stock Show
