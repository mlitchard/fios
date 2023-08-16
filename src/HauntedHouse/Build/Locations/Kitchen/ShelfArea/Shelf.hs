module HauntedHouse.Build.Locations.Kitchen.ShelfArea.Shelf
  where

import qualified Data.List.NonEmpty
import HauntedHouse.Game.Model
    ( GameState(_world'), GameStateExceptT )
import HauntedHouse.Game.Model.Mapping (GIDToDataMapping (..), ContainerMap (..))
import HauntedHouse.Game.Model.World
    ( World(_objectMap'), Object(..), ContainedOn (..), Containment (..)
    , Moveability (NotMoveable)) 
import qualified Data.Map.Strict (insert, fromList, empty)
import HauntedHouse.Build.ObjectTemplate
    ( kitchenSinkGID,
      kitchenShelfGID,
      kitchenCabinetAboveShelfGID,
      kitchenCabinetBelowShelfGID )
import HauntedHouse.Build.LocationTemplate (kitchenGID)
import Data.These (These(..))
import HauntedHouse.Build.DescriptiveTemplate ( kitchenLabel ) 

buildKitchenShelf :: GameStateExceptT ()
buildKitchenShelf =  do
  world <- _world' <$> get
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping $ Data.Map.Strict.insert kitchenShelfGID buildShelf
          $ (_unGIDToDataMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}}) 

buildShelf :: Object 
buildShelf= Object 
  { _shortName' = "A shelf next to a sink"
  , _moveability' = NotMoveable
  , _containment' = (Just . Left) shelfContainer   
  , _odescription' = "It's a shelf. You can put things on it"
  , _descriptors' = [kitchenLabel]
  }

shelfContainer :: Containment
shelfContainer = (Containment . That) (Left containedOn) 
  where 
    containedOn :: ContainedOn 
    containedOn = (ContainedOn . ContainerMap) Data.Map.Strict.empty 


