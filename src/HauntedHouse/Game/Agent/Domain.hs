module HauntedHouse.Game.Agent.Domain where

import Data.Map.Strict qualified (Map)
import HauntedHouse.Game.Agent.Atomic (AgentLabel)
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Location.Domain (LocationLabel)
import HauntedHouse.Game.Object.Container (ObjectLabel)

newtype AgentMap = AgentMap {unAgentMap :: Data.Map.Strict.Map (GID AgentLabel) AgentData}
  deriving stock (Show)

data AgentData = AgentData
  { _location :: LocationLabel
  , _inventory :: [GID ObjectLabel]
  }
  deriving stock (Show)
