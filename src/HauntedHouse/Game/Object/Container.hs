module HauntedHouse.Game.Object.Container where

import HauntedHouse.Game.Agent.Atomic (AgentLabel)
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Location.Domain (LocationLabel)
import Data.These (These)
import HauntedHouse.Game.Object.Atomic (ObjectLabel)



{-
data ContainerState
  = ContainedIn ContainedIn
  | Containing (Maybe Container) -- Nothing means not a container
  deriving stock (Show)
-}
-- Nothing means not a container or shelf
newtype ContainerState
  = ContainerState (These Container Shelf) deriving stock Show
data AttachedTo
  = AttachedToAgent (GID AgentLabel)
  | AttachedToObject (GID ObjectLabel)
  | AttachedToLocation (GID LocationLabel)
  deriving stock (Show)

data Container = Container
  { _isOpen :: Maybe Bool
  , _cinv :: [GID ObjectLabel]
  , _lockState :: Maybe LockState
  }
  deriving stock (Show)

data Moveable = Moveable | NotMovable deriving stock (Eq, Ord, Enum, Show)

data LockState = Locked | Unlocked deriving stock (Show, Eq, Ord)


data PlaceOn  = PlaceOn deriving stock Show
data PlaceUnder = PlaceUnder deriving stock Show
data PlaceAbove = PlaceAbove deriving stock Show
data Shelf    = Shelf
  { _placeability :: These (These PlaceOn PlaceAbove) PlaceUnder
  , _sinv :: Maybe (NonEmpty (GID ObjectLabel, These (These PlaceUnder PlaceOn) PlaceAbove))
  } deriving stock Show