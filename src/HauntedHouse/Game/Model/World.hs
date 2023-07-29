module HauntedHouse.Game.Model.World where 

import Data.List.NonEmpty qualified

import HauntedHouse.Game.Model.GID
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.Object.Relation
import HauntedHouse.Tokenizer (Lexeme)

-- a is Object b is Location
data Object = Object
  { _container'     :: Maybe Container
  , _containedBy'   :: Maybe ContainedBy
  , _moveability'   :: Moveablility
  , _odescription'  :: Text
  }

-- a is Object
data Container = Container
  { _isOpen     :: Maybe Bool
  , _containing :: Maybe Containing 
  , _lockState  :: Maybe LockState
  , _relatedObjects :: RelatedObjects Object
  }

data ContainedBy
  = ByObject (GID Object)
  | ByLocation (GID Location)
  | ByPlayer 

data Containing = Containing
  { _placeIn    :: Maybe (NonEmpty (GID Object))
  , _placeOn    :: Maybe (NonEmpty (GID Object))
  , _placeUnder :: Maybe (NonEmpty (GID Object))
  , _placeAbove :: Maybe (NonEmpty (GID Object))
  }

data Location = Location
  { _description  :: Text
  , _objects      :: Maybe (Data.List.NonEmpty.NonEmpty (GID Object))
  , _exits        :: Maybe (Data.List.NonEmpty.NonEmpty (GID Exit))  
  }

data World = World 
  { _objectMap'         :: GIDToDataMapping Object
  , _objectLabelMap'    :: LabelToGIDMapping Object
  , _locationMap'       :: GIDToDataMapping Location  
  , _locationLabelMap'  :: LabelToGIDMapping Location
  }

newtype Exit = Exit {_unExit :: Lexeme} deriving stock (Show,Eq,Ord)
data LockState = Locked | Unlocked deriving stock (Show, Eq, Ord)
