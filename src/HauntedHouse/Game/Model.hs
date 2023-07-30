module HauntedHouse.Game.Model where 

import Data.List.NonEmpty qualified             (NonEmpty) 
import HauntedHouse.Game.Model.World
import System.Console.Haskeline (InputT)
import HauntedHouse.Game.Model.GID (GID)

type GameStateExceptT = ExceptT Text (StateT GameState IO)
type InputGameStateExceptT = InputT GameStateExceptT

data GameState = GameState
  { _world'         :: World
  , _report'        :: [Text]
  , _player'        :: Player
  , _narration'     :: Narration
  , _newScene'      :: Bool
  , _clarification' :: Maybe (NonEmpty Text)
  } deriving stock Show

data Narration = Narration
  { _playerAction'         :: Maybe (Data.List.NonEmpty.NonEmpty Text)
  , _environmentResponse'  :: Maybe (Data.List.NonEmpty.NonEmpty Text)
  , _npAgentResponse'      :: Maybe (Data.List.NonEmpty.NonEmpty Text)
  , _scene'                :: Maybe (Data.List.NonEmpty.NonEmpty Text)
  }
  deriving stock (Show)

data Player = Player 
  { _playerLocation'  :: GID Location
  , _p_inv'           :: Maybe (Data.List.NonEmpty.NonEmpty (GID Object))
  } deriving stock Show