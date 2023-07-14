module HauntedHouse.Game.Object.Domain where

import Data.Map.Strict qualified (Map)
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Object.Container (ContainerState, Moveable
                                          , AttachedTo)
import HauntedHouse.Game.Object.Atomic (ObjectLabel)

-- import HauntedHouse.Game.Object.Container (ContainedIn, Container, ContainerState, Moveable, ObjectLabel (..))

-- _container is either (Container or Contained) or (Container and Contained)
data Object = Object
  { _container :: Maybe ContainerState 
  , _containedBy :: Maybe AttachedTo 
  , _moveability :: Moveable
  , _odescription :: Text
  }
  deriving stock (Show)

newtype ObjectMap = ObjectMap
  { _unObjectMap :: Data.Map.Strict.Map (GID ObjectLabel) Object
  }
  deriving stock (Show)

newtype ObjectLabelMap = ObjectLabelMap
  { _unObjectLabelMap :: Data.Map.Strict.Map ObjectLabel 
                                             (NonEmpty (GID ObjectLabel))
  }
  deriving stock (Show)

-- note
-- need a way to discern containment from objects point of view
