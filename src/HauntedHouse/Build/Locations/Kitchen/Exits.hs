module HauntedHouse.Build.Locations.Kitchen.Exits where

import HauntedHouse.Game.Model.Mapping ( NeighborMap(NeighborMap), Label (..))
import HauntedHouse.Game.Model.World 
import HauntedHouse.Build.LocationTemplate (hallGID)
import HauntedHouse.Build.ExitTemplate (kitchenEastExitGID)
import HauntedHouse.Game.World (setWorldExitMapM)
import HauntedHouse.Build.ObjectTemplate (kitchenEastDoorGID)
import qualified Data.Map.Strict
import HauntedHouse.Game.Object (setObjectMapM)
import HauntedHouse.Tokenizer (Lexeme(..))
import HauntedHouse.Build.DescriptiveTemplate (lockedLabel, visibleLabel)
import HauntedHouse.Game.Model.Condition (Moveability(..), Perceptibility (..))

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

kitchenEastDoorObject :: Object
kitchenEastDoorObject = Object {
      _shortName'     = kitchenShortName
    , _odescription'  = [kitchenEastDoorDesc]
    , _descriptives'  = []
    , _moveability'   = NotMoveable
    , _perceptability' = Perceptible
    , _mNexus'         = Just eastDoorNexus
  } 
  where
    kitchenShortName    = "The door to the east hall."
    kitchenEastDoorDesc = "It's a door made from some mysterious substance."

{-
newtype Nexus = Nexus {
    _unNexus' :: Either Containment Portal
  } deriving stock (Show)

data Portal = Portal {
      _portalInterface' :: Interface
    , _portalExit' :: GID Exit
  } deriving stock Show


-}
eastDoorNexus :: Nexus 
eastDoorNexus = (Nexus . Right) portal
  where 
    portal = Portal {
        _portalInterface' = ContainerInterface' kitchenEastDoorPortalInterface      
      , _portalExit' = kitchenEastExitGID -- kitchenEastDoorGID
    }

{-
data ContainerInterface = ContainerInterface {
      _openState'    :: OpenState
    , _openAction'   :: GameStateExceptT ()
    , _closeAction'  :: GameStateExceptT ()
    , _lockAction'   :: GameStateExceptT ()
    , _unlockAction' :: GameStateExceptT ()
  }
-}

kitchenEastDoorPortalInterface :: ContainerInterface
kitchenEastDoorPortalInterface = ContainerInterface {
      _openState'     = Open 
    , _openAction'    = pass 
    , _closeAction'   = pass 
    , _lockAction'    = pass 
    , _unlockAction'  = pass
  }
{-
kitchenEastDoorPortal :: Portal
kitchenEastDoorPortal = Portal 
  { _portalExit' = kitchenEastExitGID
  , _portalInterface' = kitchenEastDoorPortalInterface
  }
-}