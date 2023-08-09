module HauntedHouse.Game.Model.World where

import Data.List.NonEmpty qualified
import Data.Map.Strict (Map)
import HauntedHouse.Game.Model.GID
import HauntedHouse.Game.Model.Mapping
import Data.These

data Object = Object
  { _shortName'     :: Text
  , _moveability'   :: Moveability
  , _containment'   :: Maybe (Either Container Portal)
  , _odescription'  :: Text
  } deriving stock Show

data Moveability = Moveable | NotMoveable deriving stock (Eq, Ord, Enum, Show)

data LeftOrRight = OnLeft | OnRight deriving stock (Eq,Ord,Show)

data Location = Location
  { _title'           :: Text
  , _description'     :: Text
  , _anchoredObjects' :: RoomAnchors
  , _floorInventory'  :: Maybe Objects
  , _directions'      :: Maybe ExitGIDMap
  } deriving stock Show

data RoomAnchor
  = NorthAnchor
  | SouthAnchor
  | WestAnchor
  | EastAnchor
  | NorthWestAnchor
  | NorthEastAnchor
  | SouthWestAnchor
  | SouthEastAnchor
  | CenterAnchor
    deriving stock (Show,Eq,Ord)

newtype ObjectAnchors
          = ObjectAnchors {
              _unObjectAnchors :: Data.Map.Strict.Map (GID Object) Neighbors
            } deriving stock Show
newtype RoomAnchors
          = RoomAnchors {
              _unRoomAnchors :: Data.Map.Strict.Map RoomAnchor ObjectAnchors
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
  | PlacedLeft
  | PlacedRight
      deriving stock (Eq,Ord,Show)

newtype Neighbors = Neighbors 
  {_unNeighbors' :: NeighborMap Proximity Object} deriving stock Show 
{-
data Container = Container
  { _containerInterFace'  :: Interface Container
  , _contained'           :: These ContainedIn ContainedOn
  } deriving stock (Eq,Ord,Show)
-}

newtype Container = Container 
  {_unContainer :: These ContainedIn ContainedOn } deriving stock Show

data ContainedIn = ContainedIn 
  { _interface :: Interface Container 
  , _containedOn' :: ContainerMap Object
  } deriving stock (Eq,Ord,Show)

newtype ContainedOn = ContainedOn {_unContainedOn' :: ContainerMap Object}
  deriving stock (Eq,Ord,Show)

data Portal = Portal
  { _portalExit'      :: GID Exit
  , _portalInterface' :: Interface Portal
  } deriving stock (Eq,Ord,Show)
