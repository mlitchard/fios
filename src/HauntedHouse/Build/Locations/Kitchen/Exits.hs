module HauntedHouse.Build.Locations.Kitchen.Exits where

import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Game.Model.World ( Exit (..), Object (..)
      , Moveablility (NotMovable), LockState (Unlocked), Portal (..)
      , Relations (..), Anchor (..), Placeable (PlaceableExit), NotContainers (..))
import HauntedHouse.Build.LocationTemplate (hallGID, kitchenGID)
import HauntedHouse.Build.ExitTemplate (kitchenEastExitGID)
import HauntedHouse.Game.World (setWorldExitMapM, setObjectMapM)
import HauntedHouse.Build.ObjectTemplate (kitchenEastDoorGID)
import qualified Data.List.NonEmpty (singleton)

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
  { _related          :: Relations 
  , _moveability'     :: Moveablility
  , _odescription'    :: Text
  , _portal           :: Portal
  } deriving stock Show
-}

kitchenEastDoorObject :: Object
kitchenEastDoorObject = Object
  { _related        = objectsRelatedToEastDoor
  , _moveability'   = NotMovable
  , _odescription'  = kitchenEastDoorDesc
  , _portal'         = Just kitchenEastDoorPortal
  }
  where
    kitchenEastDoorDesc = "The door to the east hall."

{-

data Portal = Portal 
  { _lockState'   :: LockState
  , _isOpen'      :: Maybe Bool
  } deriving stock (Eq,Ord,Show)

-}
kitchenEastDoorPortal :: Portal
kitchenEastDoorPortal = Portal
  { _lockState' = Unlocked
  , _isOpen'    = Just False
  }

objectsRelatedToEastDoor :: Relations
objectsRelatedToEastDoor =
  HasAnchorAndNotContainer 
    kitchenAnchor 
    (NotContainers (PlaceableExit kitchenEastExitGID))

kitchenAnchor :: Anchor
kitchenAnchor = Anchor kitchenGID
-- HasAnchorAndNotContainer