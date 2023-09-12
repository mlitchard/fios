module HauntedHouse.Build.Locations.Kitchen.SinkArea.Cabinets.AboveSink where

import HauntedHouse.Game.Model.Mapping 
        (GIDToDataMapping (..), ContainerMap (..))
import HauntedHouse.Game.Model.World
import qualified Data.Map.Strict
import HauntedHouse.Build.ObjectTemplate (kitchenCabinetAboveSinkGID, kitchenSinkGID)
import Data.These (These(..))
import HauntedHouse.Game.Model.Condition (Moveability(..), Perceptibility (..), Proximity (PlacedAbove))

buildKitchenCabinetAboveSink :: GameStateExceptT ()
buildKitchenCabinetAboveSink = do
  world <- _world' <$> get 
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping 
          $ Data.Map.Strict.insert 
              kitchenCabinetAboveSinkGID buildCabinet 
                $ (_unGIDToDataMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}})

buildCabinet :: Object 
buildCabinet = Object { 
      _shortName' = "cabinet"
    , _odescription' = [desc]
    , _descriptives' = []
    , _moveability' = NotMoveable
    , _perceptability' = Perceptible
    , _orientation' = orientation
    , _mNexus' =  (Just . Containment') cabinetContainer
  }
  where 
    desc = "You can put things in it."

orientation :: Orientation 
orientation = AnchoredTo' (kitchenSinkGID, PlacedAbove)
cabinetContainer :: Containment
cabinetContainer = (Containment . This) containedIn

containedIn :: ContainedIn
containedIn = ContainedIn 
  {_containerInterface' = containerInterface
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
