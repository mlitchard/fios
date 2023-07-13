module HauntedHouse.Game.World.Locations (
  module HauntedHouse.Game.World.Locations
--   module HauntedHouse.Game.World.Locations.Kitchen
  ) where

import HauntedHouse.Game.World.Locations.Kitchen
import HauntedHouse.Game.Location (LocationData (..))

kitchen :: LocationData
kitchen = LocationData {
  _description = "It's a small kitchen"
, _ObjectLabelMap = Nothing 
, _exits = Nothing  
}

