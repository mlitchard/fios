module HauntedHouse.Game.Model where 

import Data.Text qualified                      (empty)
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.Object
import HauntedHouse.Game.Model.Object.Relation
import System.Console.Haskeline (InputT)
import HauntedHouse.Tokenizer (Lexeme)
import qualified Data.Map.Strict
import HauntedHouse.Game.Model.GID (GID)

-- type GameStateT a b = StateT (GameState a b) IO
type GameStateExceptT = ExceptT Text (StateT GameState IO) -- (GameStateT a b)
type InputGameStateExceptT = InputT GameStateExceptT

{-
newtype LocationMap = LocationMap
  { _unLocationMap :: Data.Map.Strict.Map (GID Location) Location }
    deriving stock (Show)
-} 
{-
newtype LocationLabelMap = LocationLabelMap
  { _unLocationLabelMap :: Data.Map.Strict.Map LocationLabel 
                                              (NonEmpty (GID Location))
  } deriving stock Show 
-}
data GameState = GameState
  { _world'         :: World
  , _report'        :: [Text]
  , _player'        :: PlayerData 
  , _narration'     :: Narration
  , _newScene'      :: Bool
  , _clarification' :: Maybe (NonEmpty Text)
  }

data Narration = Narration
  { _playerAction'         :: [Text]
  , _environmentResponse'  :: [Text]
  , _npAgentResponse'      :: [Text]
  , _scene'                :: [Text]
  }
  deriving stock (Show)

data PlayerData = PlayerData 
  { _playerLocation :: GID Location
  , _p_inv :: [GID Object]
  }

data World = World 
  { _objectMap'         :: Mapping Object
  , _locationMap'       :: Mapping Location  
  }

{-

data Location a b = Location
  { _description  :: Text
  , _objectMap    :: Mapping (Object a b) 
  , _exits        :: Mapping Exit  
  }

-}
-- newtype Mapping a = Mapping {_unMapping :: Data.Map.Strict.Map (GID a) a}
defaultLocation :: Location 
defaultLocation = Location
  { _description = Data.Text.empty 
  , _objectMap = Mapping (Data.Map.Strict.empty)
  , _exits = Mapping (Data.Map.Strict.empty)
  }

-- a is Object b is Location
defaultObject :: Object 
defaultObject = Object 
  { _container'     = Nothing
  , _containedBy'   = Nothing
  , _moveability'   = NotMovable
  , _odescription'  = Data.Text.empty 
  }