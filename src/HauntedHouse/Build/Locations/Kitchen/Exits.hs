module HauntedHouse.Build.Locations.Kitchen.Exits where

import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Game.Model.Mapping ( NeighborMap(NeighborMap))
import HauntedHouse.Game.Model.World ( Exit (..), Object (..)
      , Moveablility (NotMovable), LockState (Unlocked), Portal (..)
      , Relations (..), Container (..), Interface (..)
      , Position (Anchored), Placeability (..), LeftOrRight (..))
import HauntedHouse.Build.LocationTemplate (hallGID, kitchenGID)
import HauntedHouse.Build.ExitTemplate (kitchenEastExitGID)
import HauntedHouse.Game.World (setWorldExitMapM, setObjectMapM)
import HauntedHouse.Build.ObjectTemplate (kitchenEastDoorGID, kitchenShelfGID)
import qualified Data.List.NonEmpty (singleton)
import qualified Data.Map.Strict

buildExits :: GameStateExceptT ()
buildExits = do
  buildEastExit

{-

data World = World
  { _objectMap'         :: GIDToDataMapping Object
  , _objectLabelMap'    :: LabelToGIDListMapping Object
  , _locationMap'       :: GIDToDataMapping Location
  , _locationLabelMap'  :: LabelToGIDListMapping Location
  , _exitMap'           :: GIDToDataMapping Exit
  } deriving stock Show

-}
buildEastExit :: GameStateExceptT ()
buildEastExit = do
  setWorldExitMapM kitchenEastExitGID kitchenEastExit
  kitchenEastDoor
  pass

kitchenEastExit :: Exit
kitchenEastExit = Exit {
  _toLocation' = hallGID
  , _locationObjects' = Just $ Data.List.NonEmpty.singleton kitchenEastDoorGID
  }

kitchenEastDoor :: GameStateExceptT ()
kitchenEastDoor = do
  setObjectMapM kitchenEastDoorGID kitchenEastDoorObject

{-
data Object = Object
  { _related'       :: Relations
  , _moveability'   :: Moveablility
  , _odescription'  :: Text
  , _isContainer'   :: Maybe Container
  } deriving stock Show
-}

kitchenEastDoorObject :: Object
kitchenEastDoorObject = Object
  { _related'       = objectsRelatedToEastDoor
  , _moveability'   = NotMovable
  , _odescription'  = kitchenEastDoorDesc
  , _isContainer'   = Just kitchenEastDoorContainer
  }
  where
    kitchenEastDoorDesc = "The door to the east hall."

{-

data Container = Container
  { _containerInterFace'  :: Interface Container
  , _portal'              :: Portal
  } deriving stock (Eq,Ord,Show)

-}

kitchenEastDoorContainer :: Container 
kitchenEastDoorContainer = Container 
  { _containerInterFace' = kitchenEastDoorPortalInterface 
  , _portal' = kitchenEastDoorPortal
  }
  
kitchenEastDoorPortalInterface :: Interface Container
kitchenEastDoorPortalInterface = Interface
  {_lockState' = Unlocked
  , _isOpen'   = True
  }
-- newtype Portal
-- = Portal {_unPortal' :: GID Location} deriving stock (Eq,Ord,Show)
kitchenEastDoorPortal :: Portal
kitchenEastDoorPortal = Portal hallGID

{-

data Relations = Relations
  {_position :: Position
  , _neighbors :: Data.Map.Strict.Map (GID Object) Placeability
  } deriving stock Show

-}

objectsRelatedToEastDoor :: Relations 
objectsRelatedToEastDoor = Relations
  {_position' = objectsRelatedToEastDoorPosition
  , _neighbors' = objectsRelatedToEastDoorNeghbors
  }

objectsRelatedToEastDoorPosition :: Position
objectsRelatedToEastDoorPosition = Anchored kitchenGID

-- Data.Map.Strict.Map (GID Object) Placeability
objectsRelatedToEastDoorNeghbors :: NeighborMap Object Placeability
objectsRelatedToEastDoorNeghbors = NeighborMap
  $ Data.Map.Strict.fromList [(kitchenShelfGID, PlacedNextTo OnLeft)]

  