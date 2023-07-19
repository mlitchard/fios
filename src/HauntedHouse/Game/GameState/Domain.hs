module HauntedHouse.Game.GameState.Domain where

import HauntedHouse.Game.Location.LocationMap (LocationMap (..))
import HauntedHouse.Game.Narration.Domain (Narration)
import HauntedHouse.Game.Object.Domain (ObjectMap (..), ObjectLabelMap (..))
import HauntedHouse.Game.Player
import System.Console.Haskeline
import qualified Data.Map.Strict

type GameStateT = StateT GameState IO
type GameStateExceptT = ExceptT Text GameStateT
type InputGameStateExceptT = InputT GameStateExceptT

data GameState = GameState
  { _world          :: World
  , _report         :: [Text]
  , _player         :: PlayerData 
  , _narration      :: Narration
  , _newScene       :: Bool
  , _clarification  :: Maybe (NonEmpty Text)
  } deriving stock Show

data World = World 
  { _objectMap      :: ObjectMap
  , _objectLabelMap :: ObjectLabelMap
  , _locationMap    :: LocationMap
  } deriving stock Show 

defaultWorld :: World 
defaultWorld = World 
  { _objectMap = ObjectMap Data.Map.Strict.empty
  , _objectLabelMap = ObjectLabelMap Data.Map.Strict.empty
  , _locationMap = LocationMap Data.Map.Strict.empty
  }