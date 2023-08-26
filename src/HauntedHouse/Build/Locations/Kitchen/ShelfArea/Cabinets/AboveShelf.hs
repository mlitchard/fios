module HauntedHouse.Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf
   where

import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Model.Mapping
import qualified Data.Map.Strict
import HauntedHouse.Build.ObjectTemplate (kitchenCabinetAboveSinkGID, kitchenCabinetAboveShelfGID)
import HauntedHouse.Build.DescriptiveTemplate 
import Data.These (These(This))
import HauntedHouse.Game.Model.Condition (Moveability(..), Perceptibility (..))

buildKitchenCabinetAboveShelf :: GameStateExceptT ()
buildKitchenCabinetAboveShelf = do
  world <- _world' <$> get 
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping 
          $ Data.Map.Strict.insert 
              kitchenCabinetAboveShelfGID buildCabinet 
                $ (_unGIDToDataMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}})

buildCabinet :: Object 
buildCabinet = Object {
      _shortName' = "A cabinet, above the shelf."
    , _odescription' = [desc]
    , _descriptives' = []
    , _moveability' = NotMoveable
    , _perceptability' = Perceptible
    , _mNexus' = (Just . Nexus . Left) cabinetContainer
  }
  where 
    desc = "You can put things in it."

cabinetContainer :: Containment
cabinetContainer = (Containment . This) containedIn

containedIn :: ContainedIn
containedIn = ContainedIn 
  {_containerInterface' = ContainerInterface' containerInterface
  , _containedIn' = ContainerMap Data.Map.Strict.empty  
  }

containerInterface :: ContainerInterface 
containerInterface = ContainerInterface {
      _openState'     = Open 
    , _openAction'    = pass 
    , _closeAction'   = pass 
    , _lockAction'    = pass
    , _unlockAction'  = pass
  }