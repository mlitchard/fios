module HauntedHouse.Game.World.InitState where

import HauntedHouse.Game.World (World)
import HauntedHouse.Game.Object (ObjectLabelMap, ObjectMap, Object)
import qualified Data.Map.Strict
import HauntedHouse.Game.Labels (ObjectLabel, LocationLabel)
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Object.Container.Domain
import HauntedHouse.Game.Location (LocationMap)
import HauntedHouse.Game.Location.LocationMap (LocationLabelMap)

type InitStateT = ExceptT Text (StateT InitState IO )

data InitState = InitState {
  _world'           :: World
}


-- type BuildObject = Maybe AttachedTo -> Containing -> RelatedObjects -> Object
data InitObject = InitObject {
  _objectGID :: GID Object
  , _initFunction :: Maybe AttachedTo -> Containing -> RelatedObjects -> Object
}

data InitState' = InitState' {
  _locations :: Data.Map.Strict.Map LocationLabel [ObjectLabel]
  , _objects   :: Data.Map.Strict.Map ObjectLabel (NonEmpty (GID Object, Object))
  }