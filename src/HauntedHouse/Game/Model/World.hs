module HauntedHouse.Game.Model.World where

import Data.List.NonEmpty qualified

import HauntedHouse.Game.Model.GID
import HauntedHouse.Game.Model.Mapping
import Data.These

data Object = Object
  { _shortName'     :: Text
  , _related'       :: Relations
  , _moveability'   :: Moveablility
  , _odescription'  :: Text
  } deriving stock Show

data Moveablility = Moveable | NotMovable deriving stock (Eq, Ord, Enum, Show)

data LeftOrRight = OnLeft | OnRight deriving stock (Eq,Ord,Show)

data Location = Location
  { _title'       :: Text
  , _description' :: Text
  , _objects'     :: Maybe Objects
  , _directions'  :: Maybe ExitGIDMap
  } deriving stock Show

newtype ExitGIDMap
  = ExitGIDMap {_unExitGIDMap' :: LabelToGIDMapping Exit Object}
      deriving stock Show

data OpenClosed = Open | Closed LockState deriving stock (Eq,Ord,Show)

newtype Interface a = Interface 
  { _openState'  :: Maybe OpenClosed } deriving stock (Eq,Ord,Show)

newtype Exit = Exit { _toLocation' :: GID Location} deriving stock Show

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

-- Containment and Proximity seperate concepts
data Proximity
  = PlacedOn
  | PlacedUnder
  | PlacedAbove
  | PlacedNextTo LeftOrRight
      deriving stock (Eq,Ord,Show)

data Position
  = Anchored (GID Location)
  | Inventory
  | ContainedBy (GID Object)
  | VoidlessVoid 
      deriving stock (Show)

data Relations = Relations
  {_position'     :: Position
  , _neighbors'   :: NeighborMap Object Proximity
  , _containment' :: Maybe (Either Container Portal) 
  } deriving stock Show

data Container = Container
  { _containerInterFace'  :: Interface Container
  , _contained'           :: These ContainedIn ContainedOn
  } deriving stock (Eq,Ord,Show)

newtype ContainedIn = ContainedIn {_unContainedIn' :: ContainerMap Object}
  deriving stock (Eq,Ord,Show)

newtype ContainedOn = ContainedOn {_unContainedOn' :: ContainerMap Object}
  deriving stock (Eq,Ord,Show)

data Portal = Portal 
  { _portalExit'      :: GID Exit
  , _portalInterface' :: Interface Portal
  } deriving stock (Eq,Ord,Show)
