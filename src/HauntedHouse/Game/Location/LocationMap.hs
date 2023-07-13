module HauntedHouse.Game.Location.LocationMap where

import Data.Map.Strict qualified (Map)
import HauntedHouse.Game.Location.Domain (LocationLabel)
import HauntedHouse.Game.Location.LocationData (LocationData)

newtype LocationMap = LocationMap
  { unLocationMap :: Data.Map.Strict.Map LocationLabel LocationData
  }
  deriving stock (Show)
