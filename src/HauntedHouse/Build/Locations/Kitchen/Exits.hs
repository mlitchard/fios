module HauntedHouse.Build.Locations.Kitchen.Exits where

import HauntedHouse.Game.Model.World
import HauntedHouse.Build.ExitTemplate (kitchenEastExitGID)
import HauntedHouse.Game.World (setWorldExitMapM, setLocationDirectionM)
import HauntedHouse.Build.ObjectTemplate (kitchenEastDoorGID)
import HauntedHouse.Game.Object
    ( setObjectMapM, setObjectLabelMapM )
import HauntedHouse.Game.Model.Condition (Moveability(..), Perceptibility (..))
import HauntedHouse.Build.ObjectLabels
import HauntedHouse.Build.LocationTemplate
import HauntedHouse.Build.DirectionTemplate (eastLabel)

buildExits :: GameStateExceptT ()
buildExits =
  buildEastExit

buildEastExit :: GameStateExceptT ()
buildEastExit = do
  setWorldExitMapM kitchenEastExitGID kitchenEastExit
  kitchenEastDoor

kitchenEastExit :: Exit
kitchenEastExit = Exit {_toDestination' = hallGID }

kitchenEastDoor :: GameStateExceptT ()
kitchenEastDoor = do
  setObjectMapM kitchenEastDoorGID kitchenEastDoorObject
  setObjectLabelMapM kitchenGID door kitchenEastDoorGID
  setLocationDirectionM  kitchenGID eastLabel kitchenEastDoorGID

kitchenEastDoorObject :: Object
kitchenEastDoorObject = Object {
      _shortName'      = kitchenShortName
    , _odescription'   = [kitchenEastDoorDesc]
    , _descriptives'   = []
    , _moveability'    = NotMoveable
    , _perceptability' = Perceptible
    , _orientation'    = orientation
    , _mNexus'         = Just eastDoorNexus
  }
  where
    kitchenShortName    = "door to the east hall."
    kitchenEastDoorDesc = "It's a door made from some mysterious substance."

orientation :: Orientation
orientation = Anchoring EastAnchor

eastDoorNexus :: Nexus
eastDoorNexus = (Nexus . Right) portal
  where
    portal = Portal {
        _portalInterface' = ContainerInterface' kitchenEastDoorPortalInterface
      , _portalExit' = kitchenEastExitGID
    }

kitchenEastDoorPortalInterface :: ContainerInterface
kitchenEastDoorPortalInterface = ContainerInterface {
      _openState'     = Open
    , _openAction'    = pass
    , _closeAction'   = pass
    , _lockAction'    = pass
    , _unlockAction'  = pass
  }