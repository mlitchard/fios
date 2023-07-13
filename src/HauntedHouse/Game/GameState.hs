module HauntedHouse.Game.GameState (
  module HauntedHouse.Game.GameState 
, HauntedHouse.Game.GameState.Domain.GameStateExceptT
, HauntedHouse.Game.GameState.Domain.GameState (..)
) where

import HauntedHouse.Game.GameState.Domain (GameState (..), GameStateExceptT)
import HauntedHouse.Game.Location
import HauntedHouse.Game.Object
import HauntedHouse.Game.Agent.Atomic (AgentLabel)

data Label
  = ObjectLabel' ObjectLabel 
  | LocationLabel' LocationLabel 
  | AgentLabel'    AgentLabel 
      deriving stock (Eq,Ord,Show)

getObjectMap :: GameStateExceptT ObjectMap
getObjectMap = _objectMap <$> get
