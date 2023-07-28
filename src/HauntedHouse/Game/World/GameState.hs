module HauntedHouse.Game.World.GameState where
  {-
import HauntedHouse.Game.Location
import qualified Data.Text (empty)
import qualified Data.Map.Strict (empty)
import HauntedHouse.Game.Narration (Narration(..))
import HauntedHouse.Game.GameState (GameState(..))
import HauntedHouse.Game.Object.Container.Domain
import HauntedHouse.Game.World (World (..))
import HauntedHouse.Game.Object (ObjectLabelMap(ObjectLabelMap))
import HauntedHouse.Game.Object.Domain (ObjectMap(ObjectMap), Object (..))
import HauntedHouse.Game.Location.LocationMap (LocationLabelMap(LocationLabelMap))
import HauntedHouse.Game.Player (PlayerData (..))
import HauntedHouse.Game.World.Locations (kitchenGID)
-}
{-
defaultLocation :: Location 
defaultLocation = Location
  { _description = Data.Text.empty 
  , _objectLabelMap = ObjectLabelMap Data.Map.Strict.empty 
  , _exits = ExitMap Data.Map.Strict.empty 
  }

defaultNarration :: Narration
defaultNarration = Narration 
  { _playerAction'        = [Data.Text.empty]
  , _environmentResponse' = [Data.Text.empty]
  , _npAgentResponse'     = [Data.Text.empty]
  , _scene'               = [Data.Text.empty]
  }

defaultGameState :: GameState
defaultGameState = GameState 
  { _world'         = defaultWorld 
  , _report'        = ["Let the game begin"]
  , _player'        = defaultPlayerData
  , _narration'     = defaultNarration
  , _newScene'      = True 
  , _clarification' = Nothing
  }

defaultWorld :: World 
defaultWorld = World 
  { _objectMap'       = ObjectMap Data.Map.Strict.empty
  , _objectLabelMap'  = ObjectLabelMap Data.Map.Strict.empty
  , _locationMap'     = LocationMap Data.Map.Strict.empty
  , _locationLabelMap' = LocationLabelMap Data.Map.Strict.empty 
  }

defaultPlayerData :: PlayerData 
defaultPlayerData = PlayerData { 
  _playerLocation = kitchenGID 
  , _p_inv = []
  }

defaultObject :: Object
defaultObject = Object
  { _container'   = Nothing
  , _containedBy' = Nothing
  , _moveability' = NotMovable
  , _odescription' = Data.Text.empty
  }  
{-

data Object = Object
  { _container :: Maybe Container 
  , _containedBy :: Maybe AttachedTo 
  , _moveability :: Moveable
  , _odescription :: Text
  }
  deriving stock (Show)

-}


-}