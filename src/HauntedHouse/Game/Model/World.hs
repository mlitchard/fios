module HauntedHouse.Game.Model.World where

import Data.List.NonEmpty qualified

import HauntedHouse.Game.Model.GID
import HauntedHouse.Game.Model.Mapping
-- import HauntedHouse.Game.Model.Object.Relation
import qualified Data.Map.Strict

-- a is Object b is Location
data Object = Object
  { _related'       :: Relations
  , _moveability'   :: Moveablility
  , _odescription'  :: Text
  , _isContainer'   :: Maybe Container
  } deriving stock Show

data Moveablility = Moveable | NotMovable deriving stock (Eq, Ord, Enum, Show)

data LeftOrRight = OnLeft | OnRight deriving stock (Eq,Ord,Show)
-- a is Object

data Location = Location
  { _title'       :: Text
  , _description' :: Text
  , _objects'     :: Maybe Objects
  , _directions'  :: Maybe ExitGIDMap
  } deriving stock Show

newtype ExitGIDMap
  = ExitGIDMap {_unExitGIDMap' :: LabelToGIDMapping Exit Exit}
      deriving stock Show

data Interface a = Interface
  {_lockState'   :: LockState
  , _isOpen'      :: Bool
  } deriving stock (Eq,Ord,Show)

data Container = Container
  { _containerInterFace'  :: Interface Container
  , _portal'              :: Portal
  } deriving stock (Eq,Ord,Show)

newtype Portal
  = Portal {_unPortal' :: GID Location} deriving stock (Eq,Ord,Show)

data Exit = Exit
  {_toLocation'       :: GID Location
  , _locationObjects' :: Maybe (Data.List.NonEmpty.NonEmpty (GID Object))
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

data PlaceIn = PlaceIn
  {_objectsHeld' :: Map (Label Object) (Data.List.NonEmpty.NonEmpty (GID Object))
  ,_placeInProperties' :: Portal
  } deriving stock Show

data Placeability
  = PlacedOn
  | PlacedUnder
  | PlacedAbove
  | PlacedIn (Map (Label Object) (Data.List.NonEmpty.NonEmpty (GID Object)))
  | PlacedExit
  | PlacedNextTo LeftOrRight
      deriving stock (Eq,Ord,Show)
-- Position and Relation are seperate

data Position
  = Anchored (GID Location)
  | Inventory
  | ContainedBy (GID Object)
  | VoidlessVoid 
      deriving stock (Show)

data Relations = Relations
  {_position' :: Position
  , _neighbors' :: NeighborMap Object Placeability
  } deriving stock Show

