{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
module HauntedHouse.Build.Locations.Kitchen (buildKitchen) where

import HauntedHouse.Build.DescriptiveTemplate
import HauntedHouse.Build.ExitTemplate (kitchenEastExitGID)
import HauntedHouse.Build.LocationLabels (kitchenLocationLabel)
import HauntedHouse.Build.LocationTemplate
import HauntedHouse.Build.Locations.BuildFrame
import HauntedHouse.Recognizer (Adjective)
-- import HauntedHouse.Build.Locations.Kitchen.SinkArea.Sink

import HauntedHouse.Game.Model.World
       
import qualified Data.List.NonEmpty (singleton, NonEmpty, fromList)
import qualified Data.Map.Strict
 
import HauntedHouse.Build.Locations.Kitchen.Exits ( buildExits )
import HauntedHouse.Build.Locations.Kitchen.SinkArea.Sink 
import HauntedHouse.Build.ObjectLabels 
        (cabinetLabel, kitchenSinkLabel, kitchenShelfLabel, floorLabel
        , plantPotLabel)
import HauntedHouse.Build.ObjectTemplate
        (kitchenCabinetAboveSinkGID, kitchenCabinetBelowSinkGID
        , kitchenCabinetAboveShelfGID, kitchenCabinetBelowShelfGID
        , kitchenEastDoorGID, kitchenShelfGID, kitchenSinkGID, plantPotGID, kitchenFloorGID, bagOfSoilGID)
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Mapping
        (LabelToGIDMapping (LabelToGIDMapping), Label (..)
        , NeighborMap (..), LabelToGIDListMapping (..), GIDList)
import HauntedHouse.Tokenizer ( Lexeme(..) )
import HauntedHouse.Build.Locations.Kitchen.ShelfArea.Shelf (buildKitchenShelf)
import HauntedHouse.Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf
    ( buildKitchenCabinetAboveShelf )
import HauntedHouse.Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf
    ( buildKitchenCabinetBelowShelf )
import HauntedHouse.Build.Locations.Kitchen.SinkArea.Cabinets.AboveSink
    ( buildKitchenCabinetAboveSink )
import HauntedHouse.Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink
import HauntedHouse.Game.Model.Condition (Proximity (..))
import HauntedHouse.Build.Locations.Kitchen.PlantPot (buildPlantPot)
import HauntedHouse.Build.Locations.Kitchen.Floor (buildKitchenFloor)

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
  , _anchoredTo' = kitchenAnchoredTo
  , _anchoredObjects' = kitchenAnchors
  , _objectLabelMap' = kitchenObjectLabelMap
  , _directions' = Just directions
  }

kitchenAnchoredTo :: AnchoredTo
kitchenAnchoredTo = 
  AnchoredTo $ Data.Map.Strict.fromList anchoredToList

anchoredToList :: [(GID Object,(GID Object,Proximity))]
anchoredToList = 
  [(kitchenCabinetAboveShelfGID,(kitchenShelfGID,PlacedAbove))
  , (kitchenCabinetBelowShelfGID, (kitchenShelfGID,PlacedUnder))
  , (kitchenCabinetAboveSinkGID,(kitchenSinkGID,PlacedAbove))
  , (kitchenCabinetBelowSinkGID, (kitchenSinkGID,PlacedUnder))] 
  
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
  [ (HauntedHouse.Build.ObjectLabels.cabinetLabel,kitchenCabinets)
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
 
kitchenAnchors :: RoomAnchors 
kitchenAnchors = RoomAnchors $ Data.Map.Strict.fromList anchorList 

anchorList :: [(RoomAnchor,ObjectAnchors)]
anchorList = [(EastAnchor, objectAnchors), (CenterAnchor,floorAnchor)]

objectAnchors :: ObjectAnchors 
objectAnchors = ObjectAnchors $ Data.Map.Strict.fromList 
  [(kitchenSinkGID,sinkNeighbors),(kitchenShelfGID,shelfNeighbors)]

floorAnchor :: ObjectAnchors 
floorAnchor = ObjectAnchors 
  $ Data.Map.Strict.singleton kitchenFloorGID noFloorNeighbors

noFloorNeighbors :: Neighbors 
noFloorNeighbors = Neighbors $ NeighborMap $ Data.Map.Strict.empty 

shelfNeighbors :: Neighbors 
shelfNeighbors = Neighbors shelfNeighborMap

shelfNeighborMap :: NeighborMap Proximity Object 
shelfNeighborMap = NeighborMap $ Data.Map.Strict.fromList shelfNeighborsList

shelfNeighborsList :: [(Proximity, GIDList Object)]
shelfNeighborsList =
  [(PlacedAbove, Data.List.NonEmpty.singleton kitchenCabinetAboveShelfGID)
  , (PlacedUnder, Data.List.NonEmpty.singleton kitchenCabinetBelowShelfGID)
  , (PlacedLeft, Data.List.NonEmpty.singleton kitchenSinkGID)
  , (PlacedRight, Data.List.NonEmpty.singleton kitchenEastDoorGID)
 ]

sinkNeighbors :: Neighbors 
sinkNeighbors = Neighbors sinkNeighborMap 

sinkNeighborMap :: NeighborMap Proximity Object 
sinkNeighborMap = NeighborMap $ Data.Map.Strict.fromList sinkNeighborsList

sinkNeighborsList :: [(Proximity, GIDList Object)]
sinkNeighborsList = 
  [(PlacedAbove, Data.List.NonEmpty.singleton kitchenCabinetAboveSinkGID)
  ,(PlacedUnder, Data.List.NonEmpty.singleton kitchenCabinetBelowSinkGID)
  ,(PlacedRight,Data.List.NonEmpty.singleton kitchenShelfGID)]

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
