module HauntedHouse.Game.Model.World where

import Data.List.NonEmpty qualified

import HauntedHouse.Game.Model.GID
import HauntedHouse.Game.Model.Mapping
-- import HauntedHouse.Game.Model.Object.Relation
import qualified Data.Map.Strict

-- a is Object b is Location
data Object = Object
  { _related          :: Relations 
  , _moveability'     :: Moveablility
  , _odescription'    :: Text
  } deriving stock Show

data Moveablility = Moveable | NotMovable deriving stock (Eq, Ord, Enum, Show)

data Placeability
  = PlaceOn
  | PlaceUnder
  | PlaceAbove
  | PlaceIn
  | PlaceExit 
  | PlaceNextTo LeftOrRight
      deriving stock (Eq,Ord,Show)

data LeftOrRight = OnLeft | OnRight deriving stock (Eq,Ord,Show)
-- a is Object

data Location = Location
  { _title'       :: Text
  , _description' :: Text
  , _objects'     :: Maybe Objects
  , _directions'  :: Maybe ExitGIDMap
  } deriving stock Show

newtype ExitGIDMap
  = ExitGIDMap {_unExit' :: LabelToGIDMapping Exit Exit}
      deriving stock Show

data Exit = Exit
  { _lockState'   :: LockState
  , _isOpen'      :: Maybe Bool
  , _toLocation'  :: GID Location
  } deriving stock Show

newtype Objects
  = Objects {_unObjects' :: Data.List.NonEmpty.NonEmpty (GID Object)}
      deriving stock Show

data World = World
  { _objectMap'         :: GIDToDataMapping Object
  , _objectLabelMap'    :: LabelToGIDListMapping Object
  , _locationMap'       :: GIDToDataMapping Location
  , _locationLabelMap'  :: LabelToGIDListMapping Location
  , _exitMap'           :: GIDToDataMapping Exit
  } deriving stock Show

data LockState = Locked | Unlocked | Unlockable deriving stock (Show, Eq, Ord)

data Placeables 
  = PlaceableObjects (Data.Map.Strict.Map 
                  Placeability 
                  (Data.List.NonEmpty.NonEmpty (GID Object)))
  | PlaceableExit (GID Exit)
      deriving stock Show
                        
newtype Anchor = Anchor {_unAnchor' :: GID Location} deriving stock Show
newtype Containers = Containers {_unContainer' :: Placeables} 
                        deriving stock Show
newtype NotContainers = NotContainers {_unNotContainer :: Placeables}
                         deriving stock Show
                         
data Relations
  = VoidlessVoid
  | InventoryItem (GID Object)
  | HasAnchor Anchor
  | HasContainers Containers
  | HasNotContainers NotContainers
  | HasAnchorsAndContainers Anchor Containers
  | HasContainersAndNotContainers Containers NotContainers
  | HasAnchorAndNotContainer Anchor NotContainers
  | HasAnchorContainerAndNotContainer Anchor Containers NotContainers
      deriving stock Show 
