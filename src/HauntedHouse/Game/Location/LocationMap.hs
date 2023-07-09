module HauntedHouse.Game.Location.LocationMap where

import Data.Map.Strict qualified (Map)
import HauntedHouse.Game.Location.Domain (LocationName)
import HauntedHouse.Game.Location.LocationData (LocationData)

newtype LocationMap = LocationMap
  { unLocationMap :: Data.Map.Strict.Map LocationName LocationData
  }
  deriving stock (Show)
