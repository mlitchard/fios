module HauntedHouse.Game.Model where 

import Data.List.NonEmpty qualified             (NonEmpty) 
import Data.Text qualified                      (empty)
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Model.Object.Relation
import System.Console.Haskeline (InputT)
import HauntedHouse.Tokenizer (Lexeme)
import qualified Data.Map.Strict
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
  }

data Narration = Narration
  { _playerAction'         :: Maybe (Data.List.NonEmpty.NonEmpty Text)
  , _environmentResponse'  :: Maybe (Data.List.NonEmpty.NonEmpty Text)
  , _npAgentResponse'      :: Maybe (Data.List.NonEmpty.NonEmpty Text)
  , _scene'                :: Maybe (Data.List.NonEmpty.NonEmpty Text)
  }
  deriving stock (Show)

data Player = Player 
  { _playerLocation :: GID Location
  , _p_inv :: Maybe (Data.List.NonEmpty.NonEmpty (GID Object))
  }