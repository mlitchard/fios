module HauntedHouse.Game.World where 

import Data.Map.Strict qualified  (Map)
import HauntedHouse.Game.Object (ObjectMap (..), ObjectLabelMap (..))
import HauntedHouse.Game.Location.LocationMap (LocationMap (..)
                                              , LocationLabelMap (..))
import HauntedHouse.Game.Labels (ObjectLabel)
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Object.Domain (Object)

data World = World 
  { _objectMap'         :: ObjectMap
  , _locationMap'       :: LocationMap  
  } deriving stock Show 


  
unwrapObjectLabelMap :: World
                          -> Data.Map.Strict.Map ObjectLabel
                                                  (NonEmpty (GID Object))
unwrapObjectLabelMap (World _ (ObjectLabelMap objectLabelMap) _ _ ) =
  objectLabelMap