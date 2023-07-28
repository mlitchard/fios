module HauntedHouse.Game.World where

import HauntedHouse.Game.Model
import HauntedHouse.Game.Model.Mapping 
import HauntedHouse.Game.Model.Object

getObjectMap :: GameStateExceptT (Mapping Object)
getObjectMap = _objectMap' . _world' <$> get