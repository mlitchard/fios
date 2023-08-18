module HauntedHouse.Build.Locations.Kitchen.Exits where

import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Game.Model.Mapping ( NeighborMap(NeighborMap), Label (..))
import HauntedHouse.Game.Model.World ( Exit (..), Object (..)
      , Moveability (NotMoveable), LockState (Unlocked), Portal (..)
      , Containment (..), Interface (..), LeftOrRight (..), Proximity (..))
import HauntedHouse.Build.LocationTemplate (hallGID)
import HauntedHouse.Build.ExitTemplate (kitchenEastExitGID)
import HauntedHouse.Game.World (setWorldExitMapM)
import HauntedHouse.Build.ObjectTemplate (kitchenEastDoorGID)
import qualified Data.Map.Strict
import HauntedHouse.Game.Object (setObjectMapM)
import HauntedHouse.Tokenizer (Lexeme(..))
import HauntedHouse.Build.DescriptiveTemplate (lockedLabel, visibleLabel)

buildExits :: GameStateExceptT ()
buildExits =   
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
  , _moveability'   = NotMoveable
  , _containment'   = (Just . Right) kitchenEastDoorPortal 
  , _odescription'  = kitchenEastDoorDesc
  , _conditions'   = [lockedLabel, visibleLabel]
  }
  where
    kitchenShortName    = "The door to the east hall."
    kitchenEastDoorDesc = "It's a door made from some mysterious substance."

kitchenEastDoorPortalInterface :: Interface Portal
kitchenEastDoorPortalInterface = Open

kitchenEastDoorPortal :: Portal
kitchenEastDoorPortal = Portal 
  { _portalExit' = kitchenEastExitGID
  , _portalInterface' = kitchenEastDoorPortalInterface
  }