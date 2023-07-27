module HauntedHouse.Game.Location.LocationData where

import Data.Map.Strict qualified (empty)
import Data.Text qualified (empty)
import HauntedHouse.Game.Object (ObjectMap)


data Location = Location
  { _description  :: Text
  , _objectMap    :: ObjectMap
  , _exits        :: ExitMap 
  }
  deriving stock (Show)

newtype ExitMap = ExitMap 
  { _unExitMap :: Data.Map.Strict.Map (GID Location) Location}
   deriving stock Show

defaultLocation :: Location 
defaultLocation = Location
  { _description = Data.Text.empty 
  , _objectLabelMap = ObjectLabelMap Data.Map.Strict.empty 
  , _exits = ExitMap Data.Map.Strict.empty 
  }
