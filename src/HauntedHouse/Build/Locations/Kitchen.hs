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
import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Game.Model.World
        (Location (..), Objects (..), Object, Exit (..)
        , LockState (Unlockable), ExitGIDMap (..), Proximity (..)
        , RoomAnchor (..), RoomAnchors (..), ObjectAnchors (..)
        , Neighbors (..), Visibility (Visible))
-- import HauntedHouse.Build.Locations.Kitchen.ShelfArea.Shelf
-- import HauntedHouse.Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf
-- import HauntedHouse.Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf

import qualified Data.List.NonEmpty (singleton, NonEmpty, fromList)
import qualified Data.Map.Strict
 
import HauntedHouse.Build.Locations.Kitchen.Exits ( buildExits )
import HauntedHouse.Build.Locations.Kitchen.SinkArea.Sink 
import HauntedHouse.Build.ObjectLabels (cabinet, sink, shelf)
import HauntedHouse.Build.ObjectTemplate 
        (kitchenCabinetAboveSinkGID, kitchenCabinetBelowSinkGID
        , kitchenCabinetAboveShelfGID, kitchenCabinetBelowShelfGID
        , kitchenEastDoorGID, kitchenShelfGID, kitchenSinkGID)
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Mapping
        (LabelToGIDMapping (LabelToGIDMapping), Label (..)
        , NeighborMap (..), LabelToGIDListMapping (..), GIDList
        , LocationObjectList (..))
import HauntedHouse.Tokenizer ( Lexeme(..) )
import HauntedHouse.Build.Locations.Kitchen.ShelfArea.Shelf (buildKitchenShelf)

buildKitchen :: GameStateExceptT ()
buildKitchen = do
  buildLocationMap kitchenGID kitchenLocation
  buildDescriptorMap descriptiveMap
  buildExits 
  buildKitchenSink
  buildKitchenShelf
 -- buildKitchenCabinetAboveShelf
 -- buildKitchenCabinetBelowShelf

kitchenDescription :: Text 
kitchenDescription = "It's a kitchen"

kitchenLocation :: Location 
kitchenLocation = Location 
  { _title' = "The Test Kitchen"
  , _description' = kitchenDescription
  , _anchoredObjects' = kitchenAnchors
  , _floorInventory' = Nothing
  , _objectLabelMap' = kitchenObjectLabelMap
  , _visibilityList' = visibilityList
  , _directions' = Just directions
  }

visibilityList :: LocationObjectList Visibility Object
visibilityList = LocationObjectList $ Data.Map.Strict.fromList 
  [(Visible, Data.List.NonEmpty.fromList objectList)]
objectList :: [GID Object] 
objectList = 
  [kitchenShelfGID
  , kitchenCabinetAboveShelfGID
  , kitchenCabinetBelowShelfGID
  , kitchenSinkGID
  , kitchenCabinetAboveSinkGID
  , kitchenCabinetBelowSinkGID] 

kitchenObjectLabelMap :: LabelToGIDListMapping Object Object
kitchenObjectLabelMap = LabelToGIDListMapping $ Data.Map.Strict.fromList 
  [(cabinet,kitchenCabinets),(sink, kitchenSink),(shelf, kitchenShelf)]

kitchenSink :: Data.List.NonEmpty.NonEmpty (GID Object)
kitchenSink = Data.List.NonEmpty.singleton kitchenSinkGID

kitchenShelf :: Data.List.NonEmpty.NonEmpty (GID Object)
kitchenShelf = Data.List.NonEmpty.singleton kitchenShelfGID 

kitchenCabinets :: Data.List.NonEmpty.NonEmpty (GID Object)
kitchenCabinets = Data.List.NonEmpty.fromList 
  [kitchenCabinetAboveSinkGID
  , kitchenCabinetBelowSinkGID
  , kitchenCabinetAboveShelfGID
  , kitchenCabinetBelowShelfGID]
 
kitchenAnchors :: RoomAnchors 
kitchenAnchors = 
  RoomAnchors $ Data.Map.Strict.fromList [(EastAnchor, objectAnchors)]

objectAnchors :: ObjectAnchors 
objectAnchors = ObjectAnchors $ Data.Map.Strict.fromList 
  [(kitchenSinkGID,sinkNeighbors),(kitchenShelfGID,shelfNeighbors)]

shelfNeighbors :: Neighbors 
shelfNeighbors = Neighbors shelfNeighborMap

shelfNeighborMap :: NeighborMap Proximity Object 
shelfNeighborMap = NeighborMap $ Data.Map.Strict.fromList shelfNeighborsList

shelfNeighborsList :: [(Proximity, GIDList Object)]
shelfNeighborsList =
  [(PlacedAbove, Data.List.NonEmpty.singleton kitchenCabinetAboveShelfGID)
  , (PlacedUnder, Data.List.NonEmpty.singleton kitchenCabinetBelowShelfGID)
  , (PlacedLeft, Data.List.NonEmpty.singleton kitchenSinkGID)]

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