{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
module HauntedHouse.Build.Locations.Kitchen (buildKitchen) where

import HauntedHouse.Build.ExitTemplate (kitchenEastExitGID)
import HauntedHouse.Build.LocationLabels (kitchenLabel)
import HauntedHouse.Build.ObjectTemplate
import HauntedHouse.Build.LocationTemplate
import HauntedHouse.Build.Locations.BuildFrame
-- import HauntedHouse.Build.Locations.Kitchen.SinkArea.Sink
import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Game.Model.World
        (Location (..), Objects (..), Object, Exit (..)
        , LockState (Unlockable), ExitGIDMap (..))
-- import HauntedHouse.Build.Locations.Kitchen.ShelfArea.Shelf
-- import HauntedHouse.Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf
-- import HauntedHouse.Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf

import qualified Data.List.NonEmpty
import qualified Data.Map.Strict

import HauntedHouse.Build.Locations.Kitchen.Exits ( buildExits )
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Mapping 
        (LabelToGIDMapping (LabelToGIDMapping), Label (..))
import HauntedHouse.Tokenizer ( Lexeme(..) )

buildKitchen :: GameStateExceptT ()
buildKitchen = do
  buildFrame kitchenGID kitchenLabel kitchenLocation
  buildExits
 -- buildKitchenSink
 -- buildKitchenShelf
 -- buildKitchenCabinetAboveShelf
 -- buildKitchenCabinetBelowShelf

kitchenDescription :: Text 
kitchenDescription = "It's a kitchen"

kitchenLocation :: Location 
kitchenLocation = 
  Location {_title' = "The Test Kitchen"
            , _description' = kitchenDescription
            , _objects' = Just kitchenObjects
            , _directions' = Just directions}

kitchenObjects :: Objects 
kitchenObjects = Objects (Data.List.NonEmpty.fromList objectList)

objectList :: [GID Object]
objectList = 
  [kitchenShelfGID
  ,kitchenSinkGID
  ,kitchenCabinetAboveShelfGID
  ,kitchenCabinetBelowShelfGID
  ,kitchenCabinetAboveSinkGID
  ,kitchenCabinetBelowSinkGID
  ,kitchenEastDoorGID]

directions :: ExitGIDMap
directions = ExitGIDMap $ LabelToGIDMapping $ Data.Map.Strict.fromList directionList 

directionList :: [(Label Exit, GID Object)]
directionList = [(kitchenEastLabel, kitchenEastDoorGID)]

kitchenEastLabel :: Label Exit 
kitchenEastLabel = Label EAST 
{-
kitchenEastExit :: Exit 
kitchenEastExit = Exit {_toLocation' = hallGID}
-}