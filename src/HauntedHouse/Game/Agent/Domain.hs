module HauntedHouse.Game.Agent.Domain where

import Data.Map.Strict qualified (Map)
import HauntedHouse.Game.Agent.Atomic (AgentName)
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Location.Domain (LocationName)
import HauntedHouse.Game.Object.Container (ObjectName)

newtype AgentMap = AgentMap {unAgentMap :: Data.Map.Strict.Map (GID AgentName) AgentData}
  deriving stock (Show)

data AgentData = AgentData
  { _location :: LocationName
  , _inventory :: [GID ObjectName]
  }
  deriving stock (Show)
