module HauntedHouse.Game.Location.LocationData where 

import HauntedHouse.Game.Object.Domain (ObjectNameMap)

data LocationData = LocationData {
    _description :: Text
  , _objectNameMaps   :: ObjectNameMap -- Map ObjectName [GID ObjectName]
  } deriving stock (Show)

