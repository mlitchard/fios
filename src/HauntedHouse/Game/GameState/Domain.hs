module HauntedHouse.Game.GameState.Domain where

import HauntedHouse.Game.Agent.Atomic (AgentName)
import HauntedHouse.Game.Agent.Domain (AgentMap)
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Location.LocationMap (LocationMap)
import HauntedHouse.Game.Narration.Domain (Narration)
import HauntedHouse.Game.Object.Domain (ObjectMap)
import System.Console.Haskeline

type GameStateT = StateT GameState IO
type GameStateExceptT = ExceptT Text GameStateT
type InputGameStateExceptT = InputT GameStateExceptT

data GameState = GameState
  { _objectMap :: ObjectMap
  , _locationMap :: LocationMap
  , _agentMap :: AgentMap
  , _report :: [Text]
  , _player :: GID AgentName
  , _narration :: Narration
  , _newScene :: Bool
  , _clarification :: Maybe (NonEmpty Text)
  }
  deriving stock (Show)
