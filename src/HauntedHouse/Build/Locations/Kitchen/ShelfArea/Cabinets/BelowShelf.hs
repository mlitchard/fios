module HauntedHouse.Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf 
  where
  

import HauntedHouse.Game.Model.Mapping (GIDToDataMapping (..), ContainerMap (..))
import HauntedHouse.Game.Model.World
import qualified Data.Map.Strict (insert, fromList, empty)

import HauntedHouse.Build.ObjectTemplate (kitchenCabinetAboveSinkGID, kitchenCabinetBelowSinkGID, kitchenCabinetBelowShelfGID)
import HauntedHouse.Build.LocationTemplate (kitchenGID)
import HauntedHouse.Build.DescriptiveTemplate
import Data.These (These(..))

buildKitchenCabinetBelowShelf :: GameStateExceptT ()
buildKitchenCabinetBelowShelf = pass {- do
  world <- _world' <$> get 
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping 
          $ Data.Map.Strict.insert 
              kitchenCabinetBelowShelfGID buildCabinet 
                $ (_unGIDToDataMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}})

buildCabinet :: Object 
buildCabinet = Object 
  { _shortName' = "A cabinet, under the sink."
  , _moveability' = NotMoveable
  , _containment' = (Just . Left) cabinetContainer
  , _odescription' = "You can put things in it."
  , _conditions'  = [kitchenLabel,unlockedLabel]}

cabinetContainer :: Containment
cabinetContainer = (Containment . This) containedIn

containedIn :: ContainedIn
containedIn = ContainedIn 
  {_interface' = Open
  , _containedIn' = ContainerMap Data.Map.Strict.empty  
  }
  -}