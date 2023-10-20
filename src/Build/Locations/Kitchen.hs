{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
module Build.Locations.Kitchen (buildKitchen) where

import Build.DescriptiveTemplate hiding (cabinetLabel)
import Build.LocationTemplate
import Build.Locations.BuildFrame
import Recognizer (Adjective)
-- import Build.Locations.Kitchen.SinkArea.Sink

import Game.Model.World

import qualified Data.List.NonEmpty (singleton, NonEmpty, fromList)
import qualified Data.Map.Strict

import Build.Locations.Kitchen.Exits ( buildExits )
import Build.Locations.Kitchen.SinkArea.Sink
import Build.ObjectLabels
        (cabinetLabel, kitchenSinkLabel, kitchenShelfLabel, floorLabel
        , plantPotLabel)
import Build.ObjectTemplate
        (kitchenCabinetAboveSinkGID, kitchenCabinetBelowSinkGID
        , kitchenCabinetAboveShelfGID, kitchenCabinetBelowShelfGID
        , kitchenEastDoorGID, kitchenShelfGID, kitchenSinkGID, plantPotGID, kitchenFloorGID, bagOfSoilGID, kitchenEastPortalGID)
import Game.Model.GID (GID)
import Game.Model.Mapping
        (LabelToGIDMapping (LabelToGIDMapping), Label (..)
        , LabelToGIDListMapping (..), GIDList, GIDToDataMap (..))
import Tokenizer ( Lexeme(..) )
import Build.Locations.Kitchen.ShelfArea.Shelf (buildKitchenShelf)
import Game.Model.Condition (Proximity (..))
import Build.Locations.Kitchen.FloorArea.Floor (buildKitchenFloor)
import Build.Locations.Kitchen.FloorArea.PlantPot (buildPlantPot)
import Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf.Cabinet
        (buildKitchenCabinetAboveShelf)
import Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf.Cabinet
        (buildKitchenCabinetBelowShelf)
import Build.Locations.Kitchen.SinkArea.Cabinets.AboveSink.Cabinet
        (buildKitchenCabinetAboveSink)
import Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Cabinet
        (buildKitchenCabinetBelowSink)
import qualified Data.Aeson.KeyMap as Data.List

buildKitchen :: GameStateExceptT ()
buildKitchen = do
  buildLocationMap kitchenGID kitchenLocation
  buildDescriptorMap descriptiveMap
  buildExits
  buildKitchenSink
  buildKitchenShelf
  buildKitchenFloor
  buildPlantPot
  buildKitchenCabinetAboveShelf
  buildKitchenCabinetBelowShelf
  buildKitchenCabinetAboveSink
  buildKitchenCabinetBelowSink


kitchenDescription :: Text
kitchenDescription = "It's a kitchen"

-- FIXME - when generating Locations, don't hardwire in data
kitchenLocation :: Location
kitchenLocation = Location
  { _title' = "The Test Kitchen"
  , _description' = kitchenDescription
  , _anchoredObjects' = roomAnchorMap
  , _objectLabelMap' = kitchenObjectLabelMap
  , _directions' = Just directions
  }

objectList :: [GID Object]
objectList =
  [ kitchenShelfGID
  , kitchenFloorGID
  , kitchenCabinetAboveShelfGID
  , kitchenCabinetBelowShelfGID
  , kitchenSinkGID
  , plantPotGID
  , bagOfSoilGID
  , kitchenEastDoorGID
  , kitchenCabinetAboveSinkGID
  , kitchenCabinetBelowSinkGID]

kitchenObjectLabelMap :: LabelToGIDListMapping Object Object
kitchenObjectLabelMap = LabelToGIDListMapping $ Data.Map.Strict.fromList
  [   (cabinetLabel,kitchenCabinets)
    , (kitchenSinkLabel, kitchenSink)
    , (kitchenShelfLabel, kitchenShelf)
    , (plantPotLabel, kitchenPlantPot)
    , (floorLabel,kitchenFloor )
  ]

kitchenPlantPot :: Data.List.NonEmpty.NonEmpty (GID Object)
kitchenPlantPot = Data.List.NonEmpty.singleton plantPotGID

kitchenSink :: Data.List.NonEmpty.NonEmpty (GID Object)
kitchenSink = Data.List.NonEmpty.singleton kitchenSinkGID

kitchenFloor :: Data.List.NonEmpty.NonEmpty (GID Object)
kitchenFloor = Data.List.NonEmpty.singleton kitchenFloorGID

kitchenShelf :: Data.List.NonEmpty.NonEmpty (GID Object)
kitchenShelf = Data.List.NonEmpty.singleton kitchenShelfGID

kitchenCabinets :: Data.List.NonEmpty.NonEmpty (GID Object)
kitchenCabinets = Data.List.NonEmpty.fromList
  [kitchenCabinetAboveSinkGID
  , kitchenCabinetBelowSinkGID
  , kitchenCabinetAboveShelfGID
  , kitchenCabinetBelowShelfGID]

-- Source of truth 
roomAnchorMap :: RoomSectionMap 
roomAnchorMap = Data.Map.Strict.fromList roomAnchorList

roomAnchorList :: [(RoomSection,ObjectAnchors)]
roomAnchorList = [
      (EastSection,eastAnchorObjectAnchors)
    , (FloorSection,floorAnchorObjectAnchors)
  ]

eastAnchorObjectAnchors :: ObjectAnchors
eastAnchorObjectAnchors =
  ObjectAnchors (Data.Map.Strict.fromList objectAnchorListSource)

floorAnchorObjectAnchors :: ObjectAnchors 
floorAnchorObjectAnchors = ObjectAnchors  
  (Data.Map.Strict.fromList floorAnchorListSource)
  
floorAnchorListSource :: [(GID Object, Maybe (NonEmpty Anchored))]
floorAnchorListSource = [(kitchenFloorGID, Nothing)]

objectAnchorListSource :: [(GID Object, Maybe (NonEmpty Anchored))]
objectAnchorListSource = [
    (kitchenSinkGID, Just sinkAnchored)
  , (kitchenShelfGID, Just shelfAnchored)
  , (kitchenEastDoorGID, Just doorAnchored)
  ]

sinkAnchored :: NonEmpty Anchored
sinkAnchored = Data.List.NonEmpty.fromList
  [Anchored kitchenCabinetAboveSinkGID PlacedAbove
  , Anchored kitchenCabinetBelowSinkGID PlacedUnder]

shelfAnchored :: NonEmpty Anchored
shelfAnchored = Data.List.NonEmpty.fromList
  [Anchored kitchenCabinetAboveShelfGID PlacedAbove
  , Anchored kitchenCabinetBelowShelfGID PlacedUnder]

doorAnchored :: NonEmpty Anchored
doorAnchored = Data.List.NonEmpty.singleton 
  (Anchored kitchenEastPortalGID PlacedBehind)

directions :: ExitGIDMap
directions = ExitGIDMap $ LabelToGIDMapping $ Data.Map.Strict.fromList directionList

directionList :: [(Label Exit, GID Object)]
directionList = [(kitchenEastLabel, kitchenEastDoorGID)]

kitchenEastLabel :: Label Exit
kitchenEastLabel = Label EAST

descriptiveMap :: LabelToGIDListMapping Adjective Object
descriptiveMap = LabelToGIDListMapping
  $ Data.Map.Strict.fromList descriptiveList
  where
    descriptiveList :: [(Label Adjective, GIDList Object)]
    descriptiveList =
      [(unlockedLabel, unlockedObjects), (kitchenLabel, kitchenObjects)]

    kitchenObjects :: GIDList Object
    kitchenObjects = Data.List.NonEmpty.fromList objectList

    unlockedObjects :: GIDList Object
    unlockedObjects = Data.List.NonEmpty.fromList
      [kitchenCabinetAboveShelfGID
      , kitchenCabinetBelowShelfGID
      , kitchenCabinetAboveSinkGID
      , kitchenCabinetBelowSinkGID]
