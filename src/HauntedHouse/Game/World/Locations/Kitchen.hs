module HauntedHouse.Game.World.Locations.Kitchen where

import HauntedHouse.Game.Location.LocationData (LocationData (..))

{-
data LocationData = LocationData
  { _description :: Text
  , _objectNameMaps :: ObjectNameMap -- Map ObjectName (Maybe (NonEmpty GID ObjectName))
  }

-}
{-
kitchen :: LocationData
kitchen = LocationData {
  _description = "It's a small kitchen"
, _objectNameMap = kitchenObjects 
, _exits = kitchenExits 
}

kitchenObjects :: ObjectMap 
kitchenObjects = ObjectMap $ fromList [
  kitchenCabinet
  ,kitchenSink
  ,kitchenShelfCabinetBelow
  ,kitchenShelfCabinetAbove
  ,kitchenMarque]

-}