module HauntedHouse.Game.World.InitState where 

import HauntedHouse.Game.World (World)
import HauntedHouse.Game.Object (ObjectLabelMap, ObjectMap)
import HauntedHouse.Game.Location (LocationMap)

type InitStateT = ExceptT Text (StateT InitState IO ) 

data InitState = InitState {
  _objectLabelMap'  :: ObjectLabelMap 
  , _objectMap'     :: ObjectMap 
  , _locations'     :: LocationMap
  , _world'         :: World
}

