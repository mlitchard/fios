module HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.Shelf where
-- import HauntedHouse.Game.Object.Container (Shelf (..), PlaceOn (..)
--                                          , ObjectLabel (..))
import HauntedHouse.Tokenizer 

import HauntedHouse.Game.Object.Atomic (ObjectLabel (..))

kitchenShelfLabel :: ObjectLabel
kitchenShelfLabel = ObjectLabel SHELF 

{-

data Object = Object
  { _container :: Maybe ContainerState 
  , _containedBy :: Maybe AttachedTo 
  , _moveability :: Moveable
  , _odescription :: Text
  }
  deriving stock (Show)

-}
{-
kitchenShelf :: Object 
kitchenShelf = 
kitchenSinkShelf' :: Shelf 
kitchenSinkShelf = Shelf
  { _placeability = This PlaceOn 
  , _sinv = Nothing
  }
-}
