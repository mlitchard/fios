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
    ( Player(..), Narration(Narration), GameState(..) ) 
import HauntedHouse.Game.Model.GID              (GID)
import HauntedHouse.Game.Model.Mapping
    (GIDToDataMapping (..), GIDToGIDMapping (..), LabelToGIDListMapping (..))
-- import HauntedHouse.Game.Model.Object.Relation  (Moveablility (NotMovable))
import HauntedHouse.Game.Model.World
    (Object (..), Location (..), World (..), Exit, Moveablility (NotMovable), Relations (VoidlessVoid))

defaultLocation :: Location 
defaultLocation = Location
  { _title'       = Data.Text.empty 
  , _description' = Data.Text.empty 
  , _objects'     = Nothing
  , _directions'  = Nothing
  }

defaultObject :: Object 
defaultObject = Object 
  { _related        = VoidlessVoid
  , _moveability'   = NotMovable
  , _odescription'  = Data.Text.empty 
  , _portal'        = Nothing 
  }

defaultGameState :: GameState 
defaultGameState = GameState 
  { _world' = defaultWorld 
  , _report' = [] 
  , _player' = defaultPlayer
  , _narration' = defaultNarration 
  , _newScene' = True 
  , _clarification' = Nothing
  }
defaultNarration :: Narration
defaultNarration = Narration Nothing Nothing Nothing Nothing

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
  , (hallLabel, Data.List.NonEmpty.singleton hallGID)]

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