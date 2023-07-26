module HauntedHouse.Game.Pregenerated.Maps where
import HauntedHouse.Game.Object (ObjectLabelMap (..))
import HauntedHouse.Game.World.InitState (InitStateT)
import HauntedHouse.Game.World (World(_locationMap'))

{-

data World = World 
  { _objectMap'         :: ObjectMap
  , _objectLabelMap'    :: ObjectLabelMap
  , _locationMap'       :: LocationMap
  , _locationLabelMap'  :: LocationLabelMap 
  } deriving stock Show 

-}

buildWorld :: World
buildWorld = World 
  {_objectMap'        = objectMap
  , _objectLabelMap'  = objectLabelMap
  , _locationMap'     = locationMap
  , _locationLabelMap = locationLabelMap 
  }

objectMap :: ObjectMap 
objectMap = ObjectMap 
  $ fromList [(cabinetBelowSink, GID 0)
              ,(cabinetAboveSink, GID 1)
              , (cabinetBelowShelf,GID 2)
              , (cabinetAboveShelf, GID 3)
              , (shelf, GID 4)
              ]

objectLabelMap :: ObjectLabelMap 
objectLabelMap = ObjectLabelMap $ fromList [cabinets,shelf,sink]

