module HauntedHouse.Game.Object.Domain where

import Data.Map.Strict qualified (Map)
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Object.Container.Domain ( Moveable, AttachedTo
                                                  , Container)
import HauntedHouse.Game.Object.Atomic (ObjectLabel)

-- import HauntedHouse.Game.Object.Container (ContainedIn, Container, ContainerState, Moveable, ObjectLabel (..))

-- _container is either (Container or Contained) or (Container and Contained)
data Object = Object
  { _container :: Maybe Container 
  , _containedBy :: Maybe AttachedTo 
  , _moveability :: Moveable
  , _odescription :: Text
  }
  deriving stock (Show)

newtype ObjectMap = ObjectMap
  { _unObjectMap :: Data.Map.Strict.Map (GID Object) Object
  }
  deriving stock (Show)

newtype ObjectLabelMap = ObjectLabelMap
  { _unObjectLabelMap :: Data.Map.Strict.Map ObjectLabel 
                                             (NonEmpty (GID Object))
  }
  deriving stock (Show)

-- note
-- need a way to discern containment from objects point of view
