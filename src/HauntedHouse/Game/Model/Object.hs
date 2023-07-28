module HauntedHouse.Game.Model.Object where 

import HauntedHouse.Game.Model.GID
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.Object.Relation
import HauntedHouse.Tokenizer (Lexeme)

-- a is Object b is Location
data Object = Object
  { _container'     :: Maybe (Container Object)
  , _containedBy'   :: Maybe (ContainedBy Object Location)
  , _moveability'   :: Moveable
  , _odescription'  :: Text
  }

-- a is Object
data Container a = Container
  { _isOpen     :: Maybe Bool
  , _containing :: Maybe Containing 
  , _lockState  :: Maybe LockState
  , _relatedObjects :: RelatedObjects a
  }

data ContainedBy a b
  = ByObject (GID a)
  | ByLocation (GID b)
  | ByPlayer 

data Containing = Containing
  { _placeIn    :: Maybe (NonEmpty (GID Object))
  , _placeOn    :: Maybe (NonEmpty (GID Object))
  , _placeUnder :: Maybe (NonEmpty (GID Object))
  , _placeAbove :: Maybe (NonEmpty (GID Object))
  }

data Location = Location
  { _description  :: Text
  , _objectMap    :: Mapping Object
  , _exits        :: Mapping Exit  
  }
  
newtype Exit = Exit {_unExit :: Lexeme} deriving stock (Show,Eq,Ord)
data LockState = Locked | Unlocked deriving stock (Show, Eq, Ord)
