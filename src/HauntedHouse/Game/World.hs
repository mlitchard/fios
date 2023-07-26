module HauntedHouse.Game.World where 

import Data.Map.Strict qualified (empty)
import HauntedHouse.Game.Object (ObjectMap (..), ObjectLabelMap (..))
import HauntedHouse.Game.Location.LocationMap (LocationMap (..)
                                              , LocationLabelMap (..))

data World = World 
  { _objectMap'         :: ObjectMap
  , _objectLabelMap'    :: ObjectLabelMap
  , _locationMap'       :: LocationMap
  , _locationLabelMap'  :: LocationLabelMap 
  } deriving stock Show 

defaultWorld :: World 
defaultWorld = World 
  { _objectMap'       = ObjectMap Data.Map.Strict.empty
  , _objectLabelMap'  = ObjectLabelMap Data.Map.Strict.empty
  , _locationMap'     = LocationMap Data.Map.Strict.empty
  , _locationLabelMap' = LocationLabelMap Data.Map.Strict.empty 
  }