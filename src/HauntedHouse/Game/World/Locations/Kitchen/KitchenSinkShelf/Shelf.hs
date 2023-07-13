module HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.Shelf where
import HauntedHouse.Game.Object.Container (Shelf (..), PlaceOn (..))
import Data.These (These(..))


kitchenSinkShelf :: Shelf 
kitchenSinkShelf = Shelf
  { _placeability = This PlaceOn 
  , _sinv = Nothing
  }