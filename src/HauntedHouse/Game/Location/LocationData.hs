module HauntedHouse.Game.Location.LocationData where

import HauntedHouse.Game.Object.Domain (ObjectLabelMap)
import HauntedHouse.Game.Location.Exits 
data LocationData = LocationData
  { _description    :: Text
  , _ObjectLabelMap  :: Maybe ObjectLabelMap
  , _exits          :: Maybe ExitMap 
  }
  deriving stock (Show)
