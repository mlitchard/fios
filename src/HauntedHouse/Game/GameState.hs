module HauntedHouse.Game.GameState (
  module HauntedHouse.Game.GameState
, module HauntedHouse.Game.GameState.Domain
) where

import HauntedHouse.Game.GameState.Domain
import HauntedHouse.Game.Object

getObjectMap :: GameStateExceptT ObjectMap
getObjectMap = _objectMap . _world <$> get
