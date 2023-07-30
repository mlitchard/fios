module HauntedHouse.Game.Model.World where

import Data.List.NonEmpty qualified

import HauntedHouse.Game.Model.GID
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.Object.Relation

-- a is Object b is Location
data Object = Object
  { _container'     :: Maybe Container
  , _containedBy'   :: Maybe ContainedBy
  , _moveability'   :: Moveablility
  , _odescription'  :: Text
  } deriving stock Show

-- a is Object
data Container = Container
  { _isOpen'          :: Maybe Bool
  , _lockState'       :: Maybe LockState
  , _relatedObjects'  :: RelatedObjects Object
  } deriving stock Show

data ContainedBy
  = ByObject (GID Object)
  | ByLocation (GID Location)
  | ByPlayer deriving stock Show

data Location = Location
  { _title'       :: Text
  , _description' :: Text
  , _objects'     :: Maybe Objects
  , _exits'       :: Maybe (LabelToGIDMapping Exit Location)
  } deriving stock Show

newtype Objects 
  = Objects {_unObjects' :: Data.List.NonEmpty.NonEmpty (GID Object)} 
        deriving stock Show

data World = World
  { _objectMap'         :: GIDToDataMapping Object
  , _objectLabelMap'    :: LabelToGIDListMapping Object
  , _locationMap'       :: GIDToDataMapping Location
  , _locationLabelMap'  :: LabelToGIDListMapping Location
  , _exitMap'           :: GIDToGIDMapping Exit Location
  } deriving stock Show

data LockState = Locked | Unlocked deriving stock (Show, Eq, Ord)

data Exit
