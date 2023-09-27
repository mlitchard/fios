{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
module Build.Locations.Kitchen.FloorArea.Floor where

import Game.Model.World
import Build.ObjectTemplate (plantPotGID, kitchenFloorGID)
import Game.Model.Mapping
import qualified Data.Map.Strict
import qualified Data.List.NonEmpty
import Game.Model.Condition (Moveability(..), Perceptibility (..))
import Build.ObjectLabels (plantPotLabel)
import Tokenizer (Lexeme(..))
import Game.World (initContainerMapM)
import Build.Locations.Kitchen.FloorArea.Actions.Floor.Get (getAction)
import Build.Locations.Kitchen.FloorArea.Actions.Floor.Put (putAction)
import Build.Locations.Kitchen.FloorArea.Actions.Floor.NoCanDo 
        (openAction, closeAction, lockAction, unlockAction, goAction)
import Build.Locations.Kitchen.ShelfArea.Actions.Look (lookAction)
import Game.Object (setObjectMapM)

-- Anchoring RoomAnchor

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
    orientation = Anchored CenterAnchor

floorContainer :: Container
floorContainer = 
  Container
    $ ContainerMap $ Data.Map.Strict.singleton plantPotLabel floorInv
{-
floorInv :: GIDList Object
floorInv = Data.List.NonEmpty.singleton plantPotGID
-}
standardActions :: StandardActions
standardActions = StandardActions { 
    _getAction' = getAction
  , _putAction' = putAction 
  , _lookAction' = lookAction
  , _openAction' = openAction
  , _closeAction' = closeAction 
  , _lockAction' = lockAction
  , _unlockAction' = unlockAction
  , _goAction' = goAction
}

