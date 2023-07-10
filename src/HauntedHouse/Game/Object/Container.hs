module HauntedHouse.Game.Object.Container where

import HauntedHouse.Game.Agent.Atomic (AgentName)
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Location.Domain (LocationName)
import HauntedHouse.Tokenizer (Lexeme)
import Data.These (These)

newtype ObjectName = ObjectName Lexeme deriving stock (Eq, Ord, Show)

instance ToText ObjectName where
  toText :: ObjectName -> Text
  toText (ObjectName oname) = toText oname

{-
data ContainerState
  = ContainedIn ContainedIn
  | Containing (Maybe Container) -- Nothing means not a container
  deriving stock (Show)
-}
-- Nothing means not a container or shelf
newtype ContainerState 
  = ContainerState (Maybe (ContainedIn, These Container Shelf)) deriving stock Show 
data ContainedIn
  = ContainedInAgent (GID AgentName)
  | ContainedInObject (GID ObjectName)
  | ContainedInLocation (GID LocationName)
  deriving stock (Show)

data Container = Container
  { _isOpen :: Bool
  , _cinv :: Maybe (NonEmpty (GID ObjectName))
  , _lockState :: Maybe LockState
  }
  deriving stock (Show)

data Moveable = Moveable | NotMovable deriving stock (Eq, Ord, Enum, Show)

data LockState = Locked | Unlocked deriving stock (Show, Eq, Ord)

data PlaceOn  = PlaceOn deriving stock Show
data PlaceIn  = PlaceIn deriving stock Show 
data Shelf    = Shelf
  { placeablity :: These PlaceOn PlaceIn
  , _sinv :: Maybe (NonEmpty (GID ObjectName, Either PlaceOn PlaceIn))
  } deriving stock Show