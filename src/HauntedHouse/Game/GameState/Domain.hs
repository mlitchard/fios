module HauntedHouse.Game.GameState.Domain where
import HauntedHouse.Game.Object.Domain (ObjectMap)
import System.Console.Haskeline
import HauntedHouse.Game.Location

type GameStateT = StateT GameState IO
type GameStateExceptT = ExceptT Text GameStateT
type InputGameStateExceptT = InputT GameStateExceptT

data GameState = GameState
    { _objectMap      :: ObjectMap
    , _locationMap    :: LocationMap
    , _agentMap       :: AgentMap
    , _report         :: [Text]
    , _player         :: GID AgentName 
    , _sceneState     :: SceneState
    , _newScene       :: Bool
} deriving (Show)