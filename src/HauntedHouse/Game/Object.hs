module HauntedHouse.Game.Object (
  module HauntedHouse.Game.Object
, module HauntedHouse.Game.Object.Container 
, module HauntedHouse.Game.Object.Domain 

)where

import HauntedHouse.Game.Object.Container

import HauntedHouse.Game.GameState.Domain (
  GameState (_objectMap),
  GameStateExceptT,
 )
import HauntedHouse.Game.Object.Domain

getObjectMap :: GameStateExceptT ObjectMap
getObjectMap = _objectMap <$> get

-- makeObjectMap ::  
