module HauntedHouse.Game.Object.Domain where

import HauntedHouse.Tokenizer.Data ( Lexeme(..) )
import HauntedHouse.Game.GID (GID)

import HauntedHouse.Game.Object.Container (ContainerState, Container, ObjectName (..), ContainedIn, Moveable)

-- _container is either (Container or Contained) or (Container and Contained)
data Object = Object
    { _container    :: Either ContainerState (Container, ContainedIn) 
    , _contained    :: Maybe ObjectName
    , _moveability  :: Moveable
    , _odescription :: Text
    } deriving stock Show



newtype ObjectMap = ObjectMap {
    _unObjectMap :: Map (GID ObjectName) Object
} deriving stock Show

newtype ObjectNameMap = ObjectNameMap {
    _unObjectNameMap ::  Map ObjectName (NonEmpty (GID ObjectName))
} deriving stock Show 

cabinetON :: ObjectName 
cabinetON = ObjectName CABINET

-- note
-- need a way to discern containment from objects point of view