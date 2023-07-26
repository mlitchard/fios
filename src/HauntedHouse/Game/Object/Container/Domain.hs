module HauntedHouse.Game.Object.Container.Domain where

import Data.Map.Strict qualified 
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Labels (ObjectLabel, AgentLabel,LocationLabel)

data AttachedTo
  = AttachedToAgent (GID AgentLabel)
  | AttachedToObject (GID ObjectLabel)
  | AttachedToLocation (GID LocationLabel)
  deriving stock (Show)

-- Containing must be calculated and is therefore a seperate type
data Containing = Containing
  { _placeIn    :: Maybe [GID ObjectLabel]
  , _placeOn    :: Maybe [GID ObjectLabel]
  , _placeUnder :: Maybe [GID ObjectLabel]
  , _placeAbove :: Maybe [GID ObjectLabel]
  } deriving stock (Show)

data Container = Container
  { _isOpen     :: Maybe Bool
  , _containing :: Maybe Containing 
  , _lockState  :: Maybe LockState
  , _relatedObjects :: RelatedObjects
  }
  deriving stock (Show)

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