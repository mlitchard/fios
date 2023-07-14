module HauntedHouse.Game.Location.LocationData where

import Data.Map.Strict qualified (empty)
import Data.Text qualified (empty)
import HauntedHouse.Game.Object.Domain (ObjectLabelMap (..))
import HauntedHouse.Game.Location.Exits ( ExitMap (..)) 

data LocationData = LocationData
  { _description    :: Text
  , _objectLabelMap  :: ObjectLabelMap
  , _exits          :: ExitMap 
  }
  deriving stock (Show)

defaultLocationData :: LocationData 
defaultLocationData = LocationData 
  { _description = Data.Text.empty 
  , _objectLabelMap = ObjectLabelMap Data.Map.Strict.empty 
  , _exits = ExitMap Data.Map.Strict.empty 
  }
