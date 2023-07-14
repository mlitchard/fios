module HauntedHouse.Game.GameState (
  module HauntedHouse.Game.GameState
, module HauntedHouse.Game.GameState.Domain
) where

import HauntedHouse.Game.GameState.Domain
import HauntedHouse.Game.Location
import HauntedHouse.Game.Object
import HauntedHouse.Game.Agent.Atomic (AgentLabel)
import HauntedHouse.Game.Object.Atomic (ObjectLabel)

data Label
  = ObjectLabel' ObjectLabel
  | LocationLabel' LocationLabel
  | AgentLabel'    AgentLabel
      deriving stock (Eq,Ord,Show)

getObjectMap :: GameStateExceptT ObjectMap
getObjectMap = _objectMap . _world <$> get
