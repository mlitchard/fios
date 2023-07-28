module HauntedHouse.Game.Model.Object.Relation where

import Data.Map.Strict (Map)
import HauntedHouse.Game.Model.GID

data Moveable = Moveable | NotMovable deriving stock (Eq, Ord, Enum, Show)

data Placeability
  = PlaceOn
  | PlaceUnder
  | PlaceAbove
      deriving stock (Eq,Ord,Show)

-- a is Object
newtype RelatedObjects a
          = RelatedObjects (Data.Map.Strict.Map Placeability [GID a]) 
              deriving stock Show