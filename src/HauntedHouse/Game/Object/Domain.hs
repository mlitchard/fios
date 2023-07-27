module HauntedHouse.Game.Object.Domain where

import Data.Map.Strict qualified (Map)
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Location.LocationData (Location)

data Object = Object
  { _container'     :: Maybe Container 
  , _containedBy'   :: Maybe ContainedBy
  , _moveability'   :: Moveable
  , _odescription'  :: Text
  } deriving stock (Show)

data ContainedBy
  = ByObject (GID Object)
  | ByLocation (GID Location)
  | ByPlayer
  
newtype ObjectMap = ObjectMap
  { _unObjectMap :: Data.Map.Strict.Map (GID Object) Object
  }
  deriving stock (Show)

newtype ObjectLabelMap = ObjectLabelMap
  { _unObjectLabelMap :: Data.Map.Strict.Map ObjectLabel 
                                             (NonEmpty (GID Object))
  }
  deriving stock (Show)

data Containing = Containing
  { _placeIn    :: Maybe (NonEmpty (GID Object))
  , _placeOn    :: Maybe (NonEmpty (GID Object))
  , _placeUnder :: Maybe (NonEmpty (GID Object))
  , _placeAbove :: Maybe (NonEmpty (GID Object))
  } deriving stock (Show)

data Container = Container
  { _isOpen     :: Maybe Bool
  , _containing :: Maybe Containing 
  , _lockState  :: Maybe LockState
  , _relatedObjects :: RelatedObjects
  } deriving stock (Show)

data Moveable = Moveable | NotMovable deriving stock (Eq, Ord, Enum, Show)

data LockState = Locked | Unlocked deriving stock (Show, Eq, Ord)

data Placeability
  = PlaceOn
  | PlaceUnder
  | PlaceAbove
      deriving stock (Eq,Ord,Show)

newtype RelatedObjects 
          = RelatedObjects (Data.Map.Strict.Map Placeability [GID ObjectLabel]) 
              deriving stock Show
-- note
-- need a way to discern containment from objects point of view
