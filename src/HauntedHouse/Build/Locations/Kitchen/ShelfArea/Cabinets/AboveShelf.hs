module HauntedHouse.Build.Locations.Kitchen.ShelfArea.Cabinets.AboveShelf
   where

import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Model.Mapping
import qualified Data.Map.Strict
import HauntedHouse.Build.ObjectTemplate 
        (kitchenCabinetAboveShelfGID, kitchenShelfGID)
import Data.These (These(This))
import HauntedHouse.Game.Model.Condition 
        (Moveability(..), Perceptibility (..), Proximity (PlacedAbove))
import HauntedHouse.Game.Actions.Close (standardCloseM)
import HauntedHouse.Game.Actions.Open (standardOpenM)
import HauntedHouse.Game.Actions.Get (noGetM)

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
      _shortName' = "cabinet"
    , _odescription' = [desc]
    , _descriptives' = []
    , _moveability' = NotMoveable
    , _perceptability' = Perceptible
    , _orientation' = orientation
    , _mNexus' = (Just . Containment') cabinetContainer
    , _standardActions' = standardActions
  }
  where 
    desc = "You can put things in it."

standardActions :: StandardActions
standardActions = StandardActions 
  { _get' = noGetM
  , _put' = pass 
  }

orientation :: Orientation 
orientation = AnchoredTo' (kitchenShelfGID, PlacedAbove)
cabinetContainer :: Containment
cabinetContainer = (Containment . This) containedIn

containedIn :: ContainedIn
containedIn = ContainedIn 
  {_containerInterface' = containerInterface
  , _containedIn' = ContainerMap Data.Map.Strict.empty  
  }

containerInterface :: ContainerInterface 
containerInterface = ContainerInterface {
      _describe' = mempty
    , _openState'     = Open 
    , _openAction'    = standardOpenM kitchenCabinetAboveShelfGID 
    , _closeAction'   = standardCloseM kitchenCabinetAboveShelfGID
    , _lockAction'    = pass
    , _unlockAction'  = pass
  }