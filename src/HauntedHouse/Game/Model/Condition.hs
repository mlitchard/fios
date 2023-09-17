module HauntedHouse.Game.Model.Condition where 

import Prelude hiding (show)

import qualified Data.Text
import Relude (show)

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
  | PlacedIn
  | PlacedUnder
  | PlacedAbove
  | PlacedLeft
  | PlacedRight
  | PlacedFront 
  | PlacedBehind
      deriving stock (Eq,Ord,Show)

fromProximity :: Proximity -> Text 
fromProximity proximity = 
  Data.Text.toLower . snd $ Data.Text.breakOnEnd "Placed" $ show proximity