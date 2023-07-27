module HauntedHouse.Game.Location.LocationMap where

import Data.Map.Strict qualified                (Map)
import HauntedHouse.Game.Labels                 (LocationLabel)
import HauntedHouse.Game.Location.LocationData  (Location)
import HauntedHouse.Game.GID                    (GID)

newtype LocationMap = LocationMap
  { _unLocationMap :: Data.Map.Strict.Map (GID Location) Location }
    deriving stock (Show)

newtype LocationLabelMap = LocationLabelMap
  { _unLocationLabelMap :: Data.Map.Strict.Map LocationLabel 
                                              (NonEmpty (GID Location))
  } deriving stock Show 
