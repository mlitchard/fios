module HauntedHouse.Game.Build.Default where 

import Data.List qualified                      (replicate)
import Data.List.NonEmpty qualified             (NonEmpty,fromList,singleton
                                                , length,toList,zip)
import Data.Map.Strict qualified                (Map,fromList,singleton)
import Data.Text qualified                      (empty)

import HauntedHouse.Game.Build.LocationLabels
import HauntedHouse.Game.Build.LocationTemplate
import HauntedHouse.Game.Build.Objects
import HauntedHouse.Game.Build.ObjectLabels
import HauntedHouse.Game.Build.ObjectTemplate

import HauntedHouse.Game.Model 
import HauntedHouse.Game.Model.GID              (GID)
import HauntedHouse.Game.Model.Mapping          (Label, LabelToGIDMapping (..)
                                                , GIDToDataMapping (..))
import HauntedHouse.Game.Model.Object.Relation  (Moveablility (NotMovable))
import HauntedHouse.Game.Model.World            (Object (..), Location (..)
                                                , World (..))

defaultLocation :: Location 
defaultLocation = Location
  { _description = Data.Text.empty 
  , _objects = Nothing
  , _exits = Nothing
  }

defaultObject :: Object 
defaultObject = Object 
  { _container'     = Nothing
  , _containedBy'   = Nothing
  , _moveability'   = NotMovable
  , _odescription'  = Data.Text.empty 
  }

{-

data GameState = GameState
  { _world'         :: World
  , _report'        :: [Text]
  , _player'        :: Player
  , _narration'     :: Narration
  , _newScene'      :: Bool
  , _clarification' :: Maybe (NonEmpty Text)
  }

-}
defaultGameState :: GameState 
defaultGameState = GameState 
  { _world' = defaultWorld 
  , _report' = [] 
  , _player' = defaultPlayer
  , _narration' = defaultNarration 
  , _newScene' = True 
  , _clarification' = Nothing
  }
defaultNarration = Narration Nothing Nothing Nothing Nothing

defaultPlayer = Player
  {_playerLocation = kitchenGID
  , _p_inv = Nothing
  }

defaultWorld :: World
defaultWorld = World 
  { _objectMap'         = objectGIDToObjectMap
  , _objectLabelMap'    = objectLabelMap
  , _locationMap'       = labelGIDToLocationMap
  , _locationLabelMap'  = locationLabelMap
  }

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

objectLabelMap :: LabelToGIDMapping Object
objectLabelMap = (LabelToGIDMapping . Data.Map.Strict.fromList)
  [(cabinet, kitchenCabinets)
  ,(shelf, kitchenShelf)
  , (sink, kitchenSink)
  ]

locationLabelMap :: LabelToGIDMapping Location 
locationLabelMap = (LabelToGIDMapping . Data.Map.Strict.fromList)
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
