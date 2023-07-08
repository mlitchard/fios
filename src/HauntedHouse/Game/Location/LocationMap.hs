module HauntedHouse.Game.Location.LocationMap where 

import HauntedHouse.Game.Location.Domain
import HauntedHouse.Game.Location.LocationData

newtype LocationMap = LocationMap {
    unLocationMap :: Map LocationName LocationData
    } deriving stock (Show)