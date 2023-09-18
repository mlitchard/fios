module HauntedHouse.Build.Locations.Kitchen.Exits where

import HauntedHouse.Game.Model.World
import HauntedHouse.Build.ExitTemplate (kitchenEastExitGID)
import HauntedHouse.Game.World (setWorldExitMapM, setLocationDirectionM)
import HauntedHouse.Build.ObjectTemplate (kitchenEastDoorGID, kitchenEastPortalGID)
import HauntedHouse.Game.Object
    ( setObjectMapM, setObjectLabelMapM )
import HauntedHouse.Game.Model.Condition (Moveability(..), Perceptibility (..), Proximity (PlacedBehind))
import HauntedHouse.Build.ObjectLabels
import HauntedHouse.Build.LocationTemplate
import HauntedHouse.Build.DirectionTemplate (eastLabel)
import HauntedHouse.Game.Actions.Get (noGetM)
import Control.Monad.Except (MonadError(..))
import HauntedHouse.Game.Actions.Look (lookAt)
import HauntedHouse.Game.Model.Display (updateEnvironmentM)
import HauntedHouse.Tokenizer (Lexeme(DOOR))
import HauntedHouse.Game.Model.Mapping (Label(..))

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
  setObjectMapM kitchenEastPortalGID kitchenEastDoorObject
  setObjectLabelMapM kitchenGID door kitchenEastDoorGID
  setLocationDirectionM  kitchenGID eastLabel kitchenEastDoorGID

kitchenEastDoorObject :: Object
kitchenEastDoorObject = Object {
      _shortName'      = kitchenShortName
    , _entityLabel' = Label DOOR
    , _odescription'   = [kitchenEastDoorDesc]
    , _descriptives'   = []
    , _moveability'    = NotMoveable
    , _perceptability' = Perceptible
    , _orientation'    = orientation
    , _mNexus'         = Just eastDoorNexus
    , _standardActions' = standardActions
  }
  where
    kitchenShortName    = "door to the east hall."
    kitchenEastDoorDesc = "It's a door made from some mysterious substance."

standardActions :: StandardActions
standardActions = StandardActions
  { _get' = const pass -- noGetM
  , _put' = const pass
  , _lookIn' = const (throwError doorLookInErr)
  , _lookAt' = lookAt
  , _lookOn' = const 
                $ throwError ("There's nothing of importance on the door" :: Text)
  }
  where
    doorLookInErr = "you can't see inside the door"

orientation :: Orientation
orientation = Anchoring EastAnchor

eastDoor :: Door
eastDoor = Door
  {_doorInterface' = kitchenEastDoorGateInterface
  , _blockedObject' = kitchenEastPortalGID
  }
eastDoorNexus :: Nexus
eastDoorNexus = Door' eastDoor


kitchenEastPortalObject :: Object
kitchenEastPortalObject = Object
  { _shortName' = "The way through the east door"
  , _entityLabel' = Label DOOR
  , _odescription' = ["Walking through would lead you to the hall"]
  , _descriptives' = mempty
  , _moveability' = NotMoveable
  , _perceptability' = Perceptible -- placeholder 
  , _orientation' = AnchoredTo' (kitchenEastDoorGID, PlacedBehind)
  , _mNexus' = (Just . Portal') kitchenEastDoorPortal
  , _standardActions' = standardActions
  }

kitchenEastDoorPortal :: Portal
kitchenEastDoorPortal = Portal
  { _portalInterface' = kitchenEastDoorPortalInterface
  , _portalExit' = kitchenEastExitGID
  }
{-
data PortalInterface = PortalInterface 
  { _lookThrough' :: GameStateExceptT ()
  , _goThrough'   :: GameStateExceptT ()
  }
-}
kitchenEastDoorPortalInterface :: PortalInterface
kitchenEastDoorPortalInterface = PortalInterface
  { _lookThrough' = pass
  , _goThrough' = pass
  }

kitchenEastDoorGateInterface :: ContainerInterface
kitchenEastDoorGateInterface = ContainerInterface {
      _describe'      = mempty
    , _openState'     = Open
    , _openAction'    = pass
    , _closeAction'   = pass
    , _lockAction'    = pass
    , _unlockAction'  = pass
  }