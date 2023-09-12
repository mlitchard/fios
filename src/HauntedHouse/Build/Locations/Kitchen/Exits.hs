module HauntedHouse.Build.Locations.Kitchen.Exits where

import HauntedHouse.Game.Model.World
import HauntedHouse.Build.ExitTemplate (kitchenEastExitGID)
import HauntedHouse.Game.World (setWorldExitMapM, setLocationDirectionM)
import HauntedHouse.Build.ObjectTemplate (kitchenEastDoorGID)
import HauntedHouse.Game.Object
    ( setObjectMapM, setObjectLabelMapM )
import HauntedHouse.Game.Model.Condition (Moveability(..), Perceptibility (..), Proximity (PlacedBehind))
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

eastDoor :: Door 
eastDoor = Door 
  {_doorInterface' = kitchenEastDoorGateInterface
 -- , _blockedObject'  
  }
eastDoorNexus :: Nexus
eastDoorNexus = Door' eastDoor
{-
  where
    portal = Portal {
        _portalInterface' = kitchenEastDoorPortalInterface
      , _portalExit' = kitchenEastExitGID
    }
    -}

{-

data Object = Object {
    _shortName'       :: Text
  , _odescription'    :: [Text]
  , _descriptives'    :: [Label Adjective]
  , _moveability'     :: Moveability
  , _perceptability'  :: Perceptibility
  , _orientation'     :: Orientation
  , _mNexus'          :: Maybe Nexus
}

-}
kitchenEastPortalObject :: Object 
kitchenEastPortalObject = Object 
  { _shortName' = "The way through the east door"
  , _odescription' = ["Walking through would lead you to the hall"]
  , _descriptives' = mempty 
  , _moveability' = NotMoveable 
  , _perceptability' = Perceptible -- placeholder 
  , _orientation' = AnchoredTo' (kitchenEastDoorGID, PlacedBehind)
  , _mNexus' = (Just . Portal') kitchenEastDoorPortal
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
      _openState'     = Open
    , _openAction'    = pass
    , _closeAction'   = pass
    , _lockAction'    = pass
    , _unlockAction'  = pass
  }