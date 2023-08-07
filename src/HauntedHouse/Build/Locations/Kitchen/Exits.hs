module HauntedHouse.Build.Locations.Kitchen.Exits where

import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Game.Model.Mapping ( NeighborMap(NeighborMap))
import HauntedHouse.Game.Model.World ( Exit (..), Object (..)
      , Moveablility (NotMovable), LockState (Unlocked), Portal (..)
      , Relations (..), Container (..), Interface (..)
      , Position (Anchored), LeftOrRight (..), Proximity (..), OpenClosed (..))
import HauntedHouse.Build.LocationTemplate (hallGID, kitchenGID)
import HauntedHouse.Build.ExitTemplate (kitchenEastExitGID)
import HauntedHouse.Game.World (setWorldExitMapM, setObjectMapM)
import HauntedHouse.Build.ObjectTemplate (kitchenEastDoorGID, kitchenShelfGID)
import qualified Data.Map.Strict

buildExits :: GameStateExceptT ()
buildExits = do
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

{-

data Object = Object
  { _shortName'     :: Text
  , _related'       :: Relations
  , _moveability'   :: Moveablility
  , _odescription'  :: Text
  } deriving stock Show

-}
kitchenEastDoorObject :: Object
kitchenEastDoorObject = Object
  { _shortName'     = kitchenShortName
  , _related'       = objectsRelatedToEastDoor
  , _moveability'   = NotMovable
  , _odescription'  = kitchenEastDoorDesc
  }
  where
    kitchenShortName    = "The door to the east hall."
    kitchenEastDoorDesc = "It's a door made from some mysterious substance."

{-

data Relations = Relations
  {_position'     :: Position
  , _neighbors'   :: NeighborMap Object Proximity
  , _containment' :: Either Container Portal 
  } deriving stock Show

-}
kitchenEastDoorContainer :: Relations 
kitchenEastDoorContainer = Relations 
  { _position' = Anchored kitchenGID 
  , _neighbors' = objectsRelatedToEastDoorNeighbors
  }
{-
  { _containerInterFace' = kitchenEastDoorPortalInterface 
  , _portal' = kitchenEastDoorPortal
  }
 -} 
kitchenEastDoorPortalInterface :: Interface Portal
kitchenEastDoorPortalInterface = Interface
  { _openState'   = Just Open }

{-

data Portal = Portal 
  { _portalExit'      :: GID Exit
  , _portalInterface' :: Interface Portal
  } deriving stock (Eq,Ord,Show)

-}
kitchenEastDoorPortal :: Portal
kitchenEastDoorPortal = Portal 
  { _portalExit' = kitchenEastExitGID
  , _portalInterface' = kitchenEastDoorPortalInterface
  }


objectsRelatedToEastDoor :: Relations 
objectsRelatedToEastDoor = Relations
  {_position'     = Anchored kitchenGID
  , _neighbors'   = objectsRelatedToEastDoorNeighbors
  , _containment' = (Just . Right) kitchenEastDoorPortal
  }

objectsRelatedToEastDoorNeighbors :: NeighborMap Object Proximity
objectsRelatedToEastDoorNeighbors = NeighborMap
  $ Data.Map.Strict.fromList [(kitchenShelfGID, PlacedNextTo OnLeft)]