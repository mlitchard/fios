module HauntedHouse.Game.World.InitState where 

import qualified Data.Map.Strict
import HauntedHouse.Game.GID (GID (..))
import HauntedHouse.Game.World (World)
import HauntedHouse.Game.Object (Object, ObjectLabelMap)
import HauntedHouse.Game.Location (LocationMap)
import HauntedHouse.Game.Labels

type InitStateT = ExceptT Text (StateT InitState IO ) 

data InitState = InitState {
  _objects :: ObjectLabelMap 
  , _locations :: LocationMap
  , _world :: World
}

