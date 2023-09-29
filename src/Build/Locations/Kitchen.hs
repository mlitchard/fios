{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
module Build.Locations.Kitchen (buildKitchen) where

import Build.DescriptiveTemplate
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
        , kitchenEastDoorGID, kitchenShelfGID, kitchenSinkGID, plantPotGID, kitchenFloorGID, bagOfSoilGID)
import Game.Model.GID (GID)
import Game.Model.Mapping
        (LabelToGIDMapping (LabelToGIDMapping), Label (..)
        , NeighborMap (..), LabelToGIDListMapping (..), GIDList)
import Tokenizer ( Lexeme(..) )
import Build.Locations.Kitchen.ShelfArea.Shelf (buildKitchenShelf)
import Game.Model.Condition (Proximity (..))
import Build.Locations.Kitchen.FloorArea.Floor (buildKitchenFloor)
import Build.Locations.Kitchen.FloorArea.PlantPot (buildPlantPot)
import Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf.Cabinet (buildKitchenCabinetAboveShelf)
import Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf.Cabinet (buildKitchenCabinetBelowShelf)
import Build.Locations.Kitchen.SinkArea.Cabinets.AboveSink.Cabinet (buildKitchenCabinetAboveSink)
import Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink.Cabinet (buildKitchenCabinetBelowSink)

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
  , _anchoredObjects' = kitchenAnchors
  , _objectLabelMap' = kitchenObjectLabelMap
  , _directions' = Just directions
  }

objectList :: [GID Object]
objectList =
  [ kitchenShelfGID
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
  [ (cabinetLabel,kitchenCabinets)
    ,(kitchenSinkLabel, kitchenSink)
    ,(kitchenShelfLabel, kitchenShelf)
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
roomAnchorMap :: Map.Strict.Map RoomAnchor ObjectAnchorMap
roomAnchorMap = Map.Strict.fromList roomAnchorList

roomAnchorList :: [(RoomAnchor,ObjectAnchorMap)]
roomAnchorList = [(EastAnchor,eastAnchorObjectMap)]

eastAnchorObjectMap :: ObjectAnchorMap
eastAnchorObjectMap =
  (ObjectAnchorMap . GIDToDataMap) (Data.Map.Strict.fromList objectAnchorList)

objectAnchorList :: [(GID Object, Maybe (NonEmpty (GID Object)))]
objectAnchorList = map removeProximity objectAnchorList

removeProximity :: (GID Object, Maybe (NonEmpty (GID Object,Proximity)))
                      -> (GID Object, Maybe (NonEmpty (GID Object)))
removeProximity (gid,Nothing) = (gid,Nothing)
removeProximity (gid,Just xs) =
  let removedProximity = Data.Map.NonStrict.fromList $ fmap fst xs
  in (gid,Just removedProximity)

objectAnchorListSource :: [(GID Object, Maybe (NonEmpty (GID Object,Proximity)))]
objectAnchorListSource = [
    (kitchenSinkGID, Just sinkAnchored)
  , (kitchenShelfGID, Just shelfAnchored)
  , (kitchenEastDoorGID, Just doorAnchored)
  ]

sinkAnchored :: NonEmpty (GID Object,Proximity)
sinkAnchored = NonEmpty.fromList
  [(kitchenCabinetAboveSinkGID, PlacedAbove)
  , (kitchenCabinetBelowSinkGID,PlacedUnder)]

shelfAnchored :: NonEmpty (GID Object,Proximity)
shelfAnchored = NonEmpty.fromList
  [(kitchenCabinetAboveShelfGID, PlacedAbove)
  , (kitchenCabinetBelowShelfGID, PlaceUnder)]

doorAnchored :: NonEmpty (GID Object, Proximity)
doorAnchored = NonEmpty.singleton (kitchenEastPortalGID, PlacedBehind)

kitchenAnchors :: RoomAnchors
kitchenAnchors = RoomAnchors $ Data.Map.Strict.fromList anchorList

anchorList :: [(RoomAnchor,NonEmpty (GID Object))]
anchorList = [(EastAnchor, objectAnchors)]

objectAnchors :: (GID Object,NonEmpty (GID Object))
objectAnchors = (kitchenSinkGID,Data.List.NonEmpty.fromList
  [kitchenShelfGID,kitchenEastDoorGID])

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
