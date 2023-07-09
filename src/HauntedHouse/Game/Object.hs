module HauntedHouse.Game.Object where

import HauntedHouse.Game.GameState.Domain (
  GameState (_objectMap),
  GameStateExceptT,
 )
import HauntedHouse.Game.Object.Domain (ObjectMap)

getObjectMap :: GameStateExceptT ObjectMap
getObjectMap = _objectMap <$> get
