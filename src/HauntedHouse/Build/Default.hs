module HauntedHouse.Build.Default where 

import Data.List qualified                      (replicate)
import Data.List.NonEmpty qualified             (NonEmpty,fromList,singleton
                                                , length,toList,zip)
import Data.Map.Strict qualified                (fromList, empty)
import Data.Text qualified                      (empty)

import HauntedHouse.Build.LocationLabels
import HauntedHouse.Build.LocationTemplate

import HauntedHouse.Build.ObjectLabels
    ( sink, shelf, cabinet )
import HauntedHouse.Build.ObjectTemplate
    ( kitchenSinkGID,
      kitchenCabinetBelowSinkGID,
      kitchenCabinetAboveSinkGID,
      kitchenShelfGID,
      kitchenCabinetAboveShelfGID,
      kitchenCabinetBelowShelfGID )
import HauntedHouse.Build.ExitTemplate

import HauntedHouse.Game.Model
    ( Player(..), Narration(..), GameState(..), Verbosity (Loud), Scene (..) ) 
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Mapping
    (GIDToDataMapping (..), GIDToGIDMapping (..), LabelToGIDListMapping (..)
    , NeighborMap (..), LabelToGIDMapping (..))

import HauntedHouse.Game.Model.World
    (Object (..), Location (..), World (..), Exit, Moveability (NotMoveable)
    , RoomAnchors (..))

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
  { _objectMap'         = GIDToDataMapping Data.Map.Strict.empty
    , _objectLabelMap'    = LabelToGIDListMapping Data.Map.Strict.empty
    , _locationMap'       = GIDToDataMapping Data.Map.Strict.empty
    , _locationLabelMap'  = LabelToGIDListMapping Data.Map.Strict.empty
    , _exitMap'           = GIDToDataMapping Data.Map.Strict.empty 
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

defaultLocation :: Location 
defaultLocation = Location
  { _title'       = Data.Text.empty 
  , _description' = Data.Text.empty 
  , _anchoredObjects' = defaultRoomAnchors
  , _floorInventory' = Nothing 
  , _directions'  = Nothing
  }

defaultRoomAnchors :: RoomAnchors 
defaultRoomAnchors = RoomAnchors Data.Map.Strict.empty 