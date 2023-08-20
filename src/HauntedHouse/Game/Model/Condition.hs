module HauntedHouse.Game.Model.Condition where 

import HauntedHouse.Game.Model.GID (GID)

import Prelude hiding (show)
import qualified Data.Map.Strict
import qualified Data.Text
import Relude (show)

newtype AnchoredTo object = AnchoredTo 
  { _unAnchoredTo' :: Data.Map.Strict.Map (GID object) (GID object,Proximity)} 
    deriving stock (Show, Eq, Ord) 

data Condition object
  = Mobility' Moveability
  | Perceptibility' Perceptibility
  | Either Proximity (AnchoredTo object) 
  | Inventory
     deriving stock (Eq, Ord, Show)

data Inventory deriving stock Show

data Moveability
  = Moveable 
  | NotMoveable 
      deriving stock (Eq, Ord, Show)

data Perceptibility 
  = Perceptible 
  | Imperceptible 
      deriving stock (Eq, Ord, Show)

data Proximity
  = PlacedOn
  | PlacedUnder
  | PlacedAbove
  | PlacedLeft
  | PlacedRight
  | PlacedFront 
  | PlacedBack
      deriving stock (Eq,Ord,Show)

fromProximity :: Proximity -> Text 
fromProximity proximity = 
  Data.Text.toLower . snd $ Data.Text.breakOnEnd "Placed" $ show proximity