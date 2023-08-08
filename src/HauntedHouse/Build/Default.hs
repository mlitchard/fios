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
    , NeighborMap (..))

import HauntedHouse.Game.Model.World
    (Object (..), Location (..), World (..), Exit, Moveability (NotMoveable)
    , Position (VoidlessVoid), Relations (..))

defaultLocation :: Location 
defaultLocation = Location
  { _title'       = Data.Text.empty 
  , _description' = Data.Text.empty 
  , _objects'     = Nothing
  , _directions'  = Nothing
  }

defaultRelations :: Relations 
defaultRelations = Relations
  {_position'  = VoidlessVoid
  , _neighbors' = NeighborMap Data.Map.Strict.empty
  , _containment' = Nothing
  }

defaultObject :: Object 
defaultObject = Object 
  {_shortName' = Data.Text.empty 
  , _related' = defaultRelations 
  , _moveability' = NotMoveable 
  , _odescription' = Data.Text.empty  
  }
{-
defaultObject :: Object 
defaultObject = Object 
  { _related'        = defaultRelations 
  , _moveability'   = NotMovable
  , _odescription'  = Data.Text.empty 
  , _isContainer' = Nothing 
  }
-}
defaultGameState :: GameState 
defaultGameState = GameState 
  { _world' = defaultWorld 
  , _report' = [] 
  , _player' = defaultPlayer
  , _narration' = defaultNarration 
  , _verbosity = Loud
  , _clarification' = Nothing
  }

{-

data Narration = Narration
  {_playerAction' :: Data.List.NonEmpty.NonEmpty Text
  ,_enviroment'   :: Data.List.NonEmpty.NonEmpty Text
  , _npcResponse' ::Data.List.NonEmpty.NonEmpty Text
  , _scene        :: Scene
  } deriving stock Show

-}
defaultNarration :: Narration
defaultNarration = Narration {
  _playerAction' = Data.List.NonEmpty.singleton playerAction
  , _enviroment' = Data.List.NonEmpty.singleton environment
  , _npcResponse' = Data.List.NonEmpty.singleton hectorSays
  , _scene'       = defaultScene 
}
  where
    playerAction = "You look around"
    environment = "A dark forboding looms"
    hectorSays  = "You see a marquee flashing, trying to get your attention"
{-

data Scene = Scene
  {_roomTitle'         :: Text
  , _roomDescription'  :: Text
  , _anchoredObjects'  :: Data.List.NonEmpty.NonEmpty Text
  , _visibleContained' :: Data.List.NonEmpty.NonEmpty Text
  , _visibleExits'     :: Data.List.NonEmpty.NonEmpty Text
  } deriving stock Show

-}
defaultScene :: Scene 
defaultScene = Scene 
  { _roomTitle'         = Data.Text.empty 
    , _roomDescription'   = Data.Text.empty 
    , _anchoredObjects'   = emptyRoom
    , _visibleContained'  = noContained
    , _visibleExits'      = noExits
  }
  where
    noContained 
      = Data.List.NonEmpty.singleton (Data.Text.empty, Data.Text.empty)
    emptyRoom = Data.List.NonEmpty.singleton "It's an empty room"
    noExits   = Data.List.NonEmpty.singleton "You don't see any visible exits"

defaultPlayer :: Player
defaultPlayer = Player
  {_playerLocation' = kitchenGID
    , _p_inv'         = Nothing
  }

defaultWorld :: World
defaultWorld = World 
  { _objectMap'         = objectGIDToObjectMap
    , _objectLabelMap'    = objectLabelMap
    , _locationMap'       = labelGIDToLocationMap
    , _locationLabelMap'  = locationLabelMap
    , _exitMap'           = exitMap 
  }

exitMap :: GIDToDataMapping Exit
exitMap = GIDToDataMapping Data.Map.Strict.empty
  

kitchenCabinets :: Data.List.NonEmpty.NonEmpty (GID Object)
kitchenCabinets = Data.List.NonEmpty.fromList 
  [kitchenCabinetAboveShelfGID
    , kitchenCabinetAboveSinkGID
    , kitchenCabinetBelowShelfGID
    , kitchenCabinetBelowSinkGID]

kitchenShelf :: Data.List.NonEmpty.NonEmpty (GID Object)
kitchenShelf = Data.List.NonEmpty.singleton kitchenShelfGID 

kitchenSink :: Data.List.NonEmpty.NonEmpty (GID Object) 
kitchenSink = Data.List.NonEmpty.singleton kitchenSinkGID 
-- LabelToGIDListMapping
objectLabelMap :: LabelToGIDListMapping Object
objectLabelMap = (LabelToGIDListMapping . Data.Map.Strict.fromList)
  [(cabinet, kitchenCabinets)
    ,(shelf, kitchenShelf)
    , (sink, kitchenSink)
  ]

locationLabelMap :: LabelToGIDListMapping Location 
locationLabelMap = (LabelToGIDListMapping . Data.Map.Strict.fromList)
  [(kitchenLabel,Data.List.NonEmpty.singleton kitchenGID)
    , (hallLabel, Data.List.NonEmpty.singleton hallGID)
  ]

objectGIDToObjectMap :: GIDToDataMapping Object
objectGIDToObjectMap = GIDToDataMapping . Data.Map.Strict.fromList 
  $ Data.List.NonEmpty.toList 
  $ Data.List.NonEmpty.zip objectGIDList defaultObjects
  where
    objectGIDList :: Data.List.NonEmpty.NonEmpty (GID Object)
    objectGIDList = kitchenCabinets <> kitchenShelf <> kitchenSink
    defaultObjects :: Data.List.NonEmpty.NonEmpty Object
    defaultObjects = Data.List.NonEmpty.fromList
      $ Data.List.replicate 
        (Data.List.NonEmpty.length objectGIDList) defaultObject

labelGIDToLocationMap :: GIDToDataMapping Location
labelGIDToLocationMap = GIDToDataMapping . Data.Map.Strict.fromList
  $ Data.List.NonEmpty.toList 
  $ Data.List.NonEmpty.zip locationGIDList defaultLocations 
  where 
    locationGIDList = Data.List.NonEmpty.fromList [kitchenGID,hallGID]
    defaultLocations = 
      Data.List.NonEmpty.fromList
        $ replicate (Data.List.NonEmpty.length locationGIDList) defaultLocation