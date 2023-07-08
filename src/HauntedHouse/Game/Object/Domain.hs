{-# LANGUAGE InstanceSigs #-}
module HauntedHouse.Game.Object.Domain where

import HauntedHouse.Tokenizer.Data ( Lexeme(..) )
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Agent.Domain ( AgentName ) 
import HauntedHouse.Game.Location.Domain (LocationName)

newtype ObjectName = ObjectName Lexeme deriving stock (Eq,Ord,Show)

instance ToText ObjectName where
    toText :: ObjectName -> Text
    toText (ObjectName oname) = toText oname

data ContainerState
    = ContainedIn ContainedIn 
    | Containing (Maybe Container) -- Nothing means not a container
    deriving stock Show

data ContainedIn 
    = ContainedInAgent (GID AgentName)
    | ContainedInObject (GID ObjectName)
    | ContainedInLocation (GID LocationName)
    deriving stock Show

-- _container is either (Container or Contained) or (Container and Contained)
data Object = Object
    { _container    :: Either ContainerState (Container, ContainedIn) 
    , _contained    :: Maybe ObjectName
    , _moveability  :: Moveable
    , _odescription :: Text
    } deriving stock Show

data Moveable = Moveable | NotMovable deriving stock (Eq,Ord,Enum,Show)

data Container = Container {
    _isOpen :: Bool
  , _cinv :: [GID ObjectName]
  , _lockState :: Maybe LockState
} deriving stock Show

data LockState = Locked | Unlocked deriving stock (Show,Eq,Ord)

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