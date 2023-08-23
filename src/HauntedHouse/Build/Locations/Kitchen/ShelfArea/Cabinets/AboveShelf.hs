module HauntedHouse.Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf
   where

import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Model.Mapping
import qualified Data.Map.Strict
import HauntedHouse.Build.ObjectTemplate (kitchenCabinetAboveSinkGID, kitchenCabinetAboveShelfGID)
import HauntedHouse.Build.DescriptiveTemplate 
import Data.These (These(This))

buildKitchenCabinetAboveShelf :: GameStateExceptT ()
buildKitchenCabinetAboveShelf = pass {- do
  world <- _world' <$> get 
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping 
          $ Data.Map.Strict.insert 
              kitchenCabinetAboveShelfGID buildCabinet 
                $ (_unGIDToDataMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}})

buildCabinet :: Object 
buildCabinet = Object 
  { _shortName' = "A cabinet, above the sink."
  , _moveability' = NotMoveable
  , _containment' = (Just . Left) cabinetContainer
  , _odescription' = "You can put things in it."
  , _conditions'  = [kitchenLabel, unlockedLabel]}

cabinetContainer :: Containment
cabinetContainer = (Containment . This) containedIn

containedIn :: ContainedIn
containedIn = ContainedIn 
  {_interface' = Open
  , _containedIn' = ContainerMap Data.Map.Strict.empty  
  }
  -}