module HauntedHouse.Game.GameState (
  module HauntedHouse.Game.GameState
, module HauntedHouse.Game.GameState.Domain
) where

import HauntedHouse.Game.GameState.Domain (GameState (..),GameStateExceptT)
import HauntedHouse.Game.Object
import HauntedHouse.Game.World ( World(_objectMap') )

getObjectMap :: GameStateExceptT ObjectMap
getObjectMap = _objectMap' . _world' <$> get