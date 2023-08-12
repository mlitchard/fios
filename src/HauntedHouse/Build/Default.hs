module HauntedHouse.Build.Default where 

import Data.List.NonEmpty qualified             (singleton
                                                )
import Data.Map.Strict qualified                (empty)
import Data.Text qualified                      (empty)

import HauntedHouse.Build.LocationTemplate
import HauntedHouse.Game.Model
    ( Player(..), Narration(..), GameState(..), Verbosity (Loud), Scene (..) ) 
import HauntedHouse.Game.Model.Mapping
    (GIDToDataMapping (..), LabelToGIDListMapping (..), LocationObjectList (..))
import HauntedHouse.Game.Model.World
    (Location (..), World (..), RoomAnchors (..), Object, Visibility)

defaultGameState :: GameState 
defaultGameState = GameState 
  { _world' = defaultWorld 
  , _report' = [] 
  , _player' = defaultPlayer
  , _narration' = defaultNarration 
  , _verbosity = Loud
  , _clarification' = Nothing
  }

defaultWorld :: World
defaultWorld = World 
  { _objectMap'     = GIDToDataMapping Data.Map.Strict.empty
    , _locationMap' = GIDToDataMapping Data.Map.Strict.empty
    , _descriptiveMap' = LabelToGIDListMapping Data.Map.Strict.empty
    , _exitMap'     = GIDToDataMapping Data.Map.Strict.empty 
  }

defaultPlayer :: Player
defaultPlayer = Player
  {_playerLocation' = kitchenGID
    , _p_inv'         = Nothing
  }

defaultNarration :: Narration
defaultNarration = Narration
  {_playerAction' = Data.List.NonEmpty.singleton Data.Text.empty
  , _enviroment' = Data.List.NonEmpty.singleton Data.Text.empty
  , _npcResponse' = Data.List.NonEmpty.singleton Data.Text.empty
  , _scene' = defaultScene
  }

defaultScene :: Scene 
defaultScene = Scene 
  { _sceneTitle' = Data.Text.empty 
  , _sceneDescription' = Data.Text.empty
  , _sceneAnchored' = Data.List.NonEmpty.singleton Data.Text.empty
  , _sceneRelated' = Data.List.NonEmpty.singleton Data.Text.empty
  , _visibleExits' = Data.List.NonEmpty.singleton Data.Text.empty
  }

{-

data Location = Location
  { _title'           :: Text
  , _description'     :: Text
  , _anchoredObjects' :: RoomAnchors
  , _floorInventory'  :: Objects
  , _objectLabelMap'  :: LabelToGIDListMapping Object
  , _visibilityList   :: LocationObjectList Visibility Object
  , _directions'      :: Maybe ExitGIDMap
  } deriving stock Show

-}

defaultLocation :: Location 
defaultLocation = Location
  { _title'       = Data.Text.empty 
  , _description' = Data.Text.empty 
  , _anchoredObjects' = defaultRoomAnchors
  , _floorInventory' = Nothing
  , _objectLabelMap' = defaultObjectLabelMap
  , _visibilityList' = defaultVisibilityList
  , _directions'  = Nothing
  }

defaultVisibilityList :: LocationObjectList Visibility Object
defaultVisibilityList = LocationObjectList Data.Map.Strict.empty

defaultObjectLabelMap :: LabelToGIDListMapping Object Object
defaultObjectLabelMap = LabelToGIDListMapping Data.Map.Strict.empty

defaultRoomAnchors :: RoomAnchors 
defaultRoomAnchors = RoomAnchors Data.Map.Strict.empty 