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
import HauntedHouse.Game.Location (getLocation)
import HauntedHouse.Game.Model.Mapping 
        (LabelToGIDMapping (LabelToGIDMapping), Label (..))
import HauntedHouse.Tokenizer (Lexeme (..))
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Model.GID (GID)
import qualified Data.Map.Strict

buildKitchen :: GameStateExceptT ()
buildKitchen = do
  location <- kitchenLocation <$> getLocation kitchenGID 
  buildFrame kitchenGID kitchenLabel location
 -- buildKitchenSink
 -- buildKitchenShelf
 -- buildKitchenCabinetAboveShelf
 -- buildKitchenCabinetBelowShelf

kitchenDescription :: Text 
kitchenDescription = "It's a kitchen"

kitchenLocation :: Location -> Location 
kitchenLocation location = 
  location {_title' = "The Test Kitchen"
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
  ,kitchenCabinetBelowSinkGID]

directions :: ExitGIDMap
directions = ExitGIDMap $ LabelToGIDMapping $ Data.Map.Strict.fromList directionList 

directionList :: [(Label Exit, GID Exit)]
directionList = [(kitchenEastLabel, kitchenEastExitGID)]

kitchenEastLabel :: Label Exit 
kitchenEastLabel = Label EAST 

{-

newtype LabelToGIDMapping a b
  = LabelToGIDMapping
      { _unlabelToGIDListMapping :: Data.Map.Strict.Map (Label a) (GID b)}
        deriving stock Show 

data Location = Location
  { _title'       :: Text
  , _description' :: Text
  , _objects'     :: Maybe Objects
  , _directions'  :: Maybe Exits
  } deriving stock Show

newtype Exits
  = Exits {_unExit' :: LabelToGIDMapping Exit Exit}
      deriving stock Show

-}




kitchenEastExit :: Exit 
kitchenEastExit = Exit 
  { _lockState'  = Unlockable 
    , _isOpen'    = Nothing
    ,_toLocation' = hallGID}


