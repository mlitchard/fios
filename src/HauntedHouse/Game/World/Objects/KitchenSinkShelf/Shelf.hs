module HauntedHouse.Game.World.Objects.KitchenSinkShelf.Shelf where
import HauntedHouse.Game.Object.Container (Shelf (..), PlaceOn (..))
import Data.These (These(..))

kitchenSinkShelf :: Shelf 
kitchenSinkShelf = Shelf
  { _placeability = This PlaceOn 
  , _sinv = Nothing
  }