module HauntedHouse.Game.Model.Object.Relation where

import Data.Map.Strict (Map)
import HauntedHouse.Game.Model.GID
import Data.List.NonEmpty qualified

data Moveablility = Moveable | NotMovable deriving stock (Eq, Ord, Enum, Show)

data Placeability
  = PlaceOn
  | PlaceUnder
  | PlaceAbove
  | PlaceIn
  | PlaceNextTo LeftOrRight
      deriving stock (Eq,Ord,Show)

data LeftOrRight = OnLeft | OnRight deriving stock (Eq,Ord,Show)
-- a is Object
newtype RelatedObjects a
          = RelatedObjects (Data.Map.Strict.Map 
                              Placeability 
                              (Maybe (Data.List.NonEmpty.NonEmpty (GID a)))) 
              deriving stock Show