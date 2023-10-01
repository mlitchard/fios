{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
module Build.Locations.Kitchen.FloorArea.Floor where

import Game.Model.World
import Build.ObjectTemplate (kitchenFloorGID, plantPotGID)
import Game.Model.Mapping
import qualified Data.Map.Strict
import Game.Model.Condition (Moveability(..))
import Build.ObjectLabels (plantPotLabel)
import Tokenizer (Lexeme(..))
import Game.World (initContainerMapM)
import Build.Locations.Kitchen.FloorArea.Actions.Floor.Get (getAction)
import Build.Locations.Kitchen.FloorArea.Actions.Floor.Put (putAction)
import Build.Locations.Kitchen.FloorArea.Actions.Floor.NoCanDo 
        (openAction, closeAction, lockAction, unlockAction, goAction)
import Build.Locations.Kitchen.ShelfArea.Actions.Look
        ( initialLookAction ) 
import Game.Object (setObjectMapM, getAnchored)
import qualified Data.List.NonEmpty
import Build.LocationTemplate (kitchenGID)

-- Anchoring RoomSection

buildKitchenFloor :: GameStateExceptT ()
buildKitchenFloor = do
  setObjectMapM kitchenFloorGID buildFloor 
  initContainerMapM kitchenFloorGID floorContainer

buildFloor :: Object
buildFloor = Object { 
    _shortName'         = "kitchen floor."
  , _entityLabel' = Label FLOOR
  , _odescription'    = [desc]
  , _descriptives'     = []
  , _moveability'     = NotMoveable
  , _orientation'     = orientation
  , _standardActions' = standardActions
}
  where
    desc = "A non-descipt tiled kitchen floor."
    orientation = Anchor floorAnchor 

floorAnchor :: GameStateExceptT (Maybe (NonEmpty Anchored))
floorAnchor = getAnchored kitchenGID EastSection kitchenFloorGID notAnchorMsg
  where
    notAnchorMsg = show kitchenFloorGID <> " is not an anchor"

floorContainer :: Container
floorContainer = 
  Container $ Data.Map.Strict.singleton plantPotLabel floorInv

floorInv :: NonEmpty ContainedEntity
floorInv = Data.List.NonEmpty.singleton plantContainer 

plantContainer :: ContainedEntity
plantContainer = ContainedEntity {
    _containedGid' = plantPotGID
  , _placement' = On
}

standardActions :: StandardActions
standardActions = StandardActions { 
    _getAction' = getAction
  , _putAction' = putAction 
  , _lookAction' = initialLookAction
  , _openAction' = openAction
  , _closeAction' = closeAction 
  , _lockAction' = lockAction
  , _unlockAction' = unlockAction
  , _goAction' = goAction
}

