module HauntedHouse.Game.Location.LocationData where

import HauntedHouse.Game.Object.Domain (ObjectNameMap)
import HauntedHouse.Game.Location.Exits 
data LocationData = LocationData
  { _description    :: Text
  , _objectNameMap  :: Maybe ObjectNameMap
  , _exits          :: Maybe ExitMap 
  }
  deriving stock (Show)
