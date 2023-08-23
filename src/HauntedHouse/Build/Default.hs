module HauntedHouse.Build.Default where

import Data.List.NonEmpty qualified             (singleton
                                                )
import Data.Map.Strict qualified                (empty)
import Data.Text qualified                      (empty)

import HauntedHouse.Build.LocationTemplate
import HauntedHouse.Game.Model.World
    ( Player(..),
      Narration(..),
      GameState(..),
      Verbosity(Loud),
      Scene(..),
      Location(..),
      World(..),
      RoomAnchors(..),
      Object )
import HauntedHouse.Game.Model.Mapping
    (GIDToDataMapping (..), LabelToGIDListMapping (..), LocationObjectList (..))

defaultGameState :: GameState
defaultGameState = GameState
  { _world' = defaultWorld
  , _report' = []
  , _player' = defaultPlayer
  , _narration' = defaultNarration
  , _verbosity' = Loud
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
{-

data SceneAnchored = SceneAnchored {
  _sceneAnchored' :: Text
, _sceneRelated' :: [Text] 
} deriving stock Show

data Scene = Scene
  {_sceneTitle'         :: Text
  , _sceneDescription'  :: Text
  , _roomAnchored'      :: [(Text,[SceneAnchored])] -- text is Room area preamble
  , _floor'             :: [Text]
  , _visibleExits'      :: [Text]
  } deriving stock Show

-}
defaultScene :: Scene
defaultScene = Scene
  { _sceneTitle' = Data.Text.empty
  , _sceneDescription' = Data.Text.empty
  , _roomAnchored' = mempty
  , _floor' = mempty
  , _visibleExits' = mempty
  }

{-

defaultLocation :: Location
defaultLocation = Location
  { _title'       = Data.Text.empty
  , _description' = Data.Text.empty
  , _anchoredObjects' = defaultRoomAnchors
  , _floorInventory' = Nothing
  , _objectLabelMap' = defaultObjectLabelMap
  , _directions'  = Nothing
  }
-}
defaultObjectLabelMap :: LabelToGIDListMapping Object Object
defaultObjectLabelMap = LabelToGIDListMapping Data.Map.Strict.empty

defaultRoomAnchors :: RoomAnchors
defaultRoomAnchors = RoomAnchors Data.Map.Strict.empty