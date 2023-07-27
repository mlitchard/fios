module HauntedHouse.Game.World where 

import Data.Map.Strict qualified (empty, Map)
import HauntedHouse.Game.Object (ObjectMap (..), ObjectLabelMap (..))
import HauntedHouse.Game.Location.LocationMap (LocationMap (..)
                                              , LocationLabelMap (..))
import HauntedHouse.Game.Labels (ObjectLabel)
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Object.Domain (Object)

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
  
unwrapObjectLabelMap :: World
                          -> Data.Map.Strict.Map ObjectLabel
                                                  (NonEmpty (GID Object))
unwrapObjectLabelMap (World _ (ObjectLabelMap objectLabelMap) _ _ ) =
  objectLabelMap