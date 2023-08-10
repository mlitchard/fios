module HauntedHouse.Build.Locations.Kitchen.Exits where

import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Game.Model.Mapping ( NeighborMap(NeighborMap))
import HauntedHouse.Game.Model.World ( Exit (..), Object (..)
      , Moveability (NotMoveable), LockState (Unlocked), Portal (..)
      , Container (..), Interface (..), LeftOrRight (..), Proximity (..))
import HauntedHouse.Build.LocationTemplate (hallGID, kitchenGID)
import HauntedHouse.Build.ExitTemplate (kitchenEastExitGID)
import HauntedHouse.Game.World (setWorldExitMapM)
import HauntedHouse.Build.ObjectTemplate (kitchenEastDoorGID, kitchenShelfGID)
import qualified Data.Map.Strict

buildExits :: GameStateExceptT ()
buildExits = pass -- do
  {-
  buildEastExit

buildEastExit :: GameStateExceptT ()
buildEastExit = do
  setWorldExitMapM kitchenEastExitGID kitchenEastExit
  kitchenEastDoor
  pass

kitchenEastExit :: Exit
kitchenEastExit = Exit {_toLocation' = hallGID }

kitchenEastDoor :: GameStateExceptT ()
kitchenEastDoor = do
  setObjectMapM kitchenEastDoorGID kitchenEastDoorObject

kitchenEastDoorObject :: Object
kitchenEastDoorObject = Object
  { _shortName'     = kitchenShortName
  , _related'       = objectsRelatedToEastDoor
  , _moveability'   = NotMoveable
  , _odescription'  = kitchenEastDoorDesc
  }
  where
    kitchenShortName    = "The door to the east hall."
    kitchenEastDoorDesc = "It's a door made from some mysterious substance."

kitchenEastDoorPortalInterface :: Interface Portal
kitchenEastDoorPortalInterface = Interface
  { _openState'   = Just Open }

kitchenEastDoorPortal :: Portal
kitchenEastDoorPortal = Portal 
  { _portalExit' = kitchenEastExitGID
  , _portalInterface' = kitchenEastDoorPortalInterface
  }

objectsRelatedToEastDoor :: Relations 
objectsRelatedToEastDoor = Relations
  {_position'     = AnchoredByRoom kitchenGID
  , _neighbors'   = objectsRelatedToEastDoorNeighbors
  , _containment' = (Just . Right) kitchenEastDoorPortal
  }

objectsRelatedToEastDoorNeighbors :: NeighborMap Object Proximity
objectsRelatedToEastDoorNeighbors = NeighborMap
  $ Data.Map.Strict.fromList [(kitchenShelfGID, PlacedNextTo OnLeft)]

-}