module HauntedHouse.Game.GameState.Domain where

import HauntedHouse.Game.Agent.Atomic (AgentLabel)
import HauntedHouse.Game.Agent.Domain (AgentMap)
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Location.LocationMap (LocationMap)
import HauntedHouse.Game.Narration.Domain (Narration)
import HauntedHouse.Game.Object.Domain (ObjectMap, ObjectLabelMap)
import System.Console.Haskeline

type GameStateT = StateT GameState IO
type GameStateExceptT = ExceptT Text GameStateT
type InputGameStateExceptT = InputT GameStateExceptT

data GameState = GameState
  { _objectMap      :: ObjectMap
  , _ObjectLabelMap  :: ObjectLabelMap
  , _locationMap    :: LocationMap
  , _agentMap       :: AgentMap
  , _report         :: [Text]
  , _player         :: GID AgentLabel
  , _narration      :: Narration
  , _newScene       :: Bool
  , _clarification  :: Maybe (NonEmpty Text)
  }
  deriving stock (Show)
