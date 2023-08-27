module HauntedHouse.Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf 
  where
  

import HauntedHouse.Game.Model.Mapping (GIDToDataMapping (..), ContainerMap (..))
import HauntedHouse.Game.Model.World
import qualified Data.Map.Strict (insert, fromList, empty)

import HauntedHouse.Build.ObjectTemplate (kitchenCabinetAboveSinkGID, kitchenCabinetBelowSinkGID, kitchenCabinetBelowShelfGID, kitchenShelfGID)
import HauntedHouse.Build.LocationTemplate (kitchenGID)
import HauntedHouse.Build.DescriptiveTemplate
import Data.These (These(..))
import HauntedHouse.Game.Model.Condition (Moveability(..), Perceptibility (..), Proximity (PlacedUnder))

buildKitchenCabinetBelowShelf :: GameStateExceptT ()
buildKitchenCabinetBelowShelf = do
  world <- _world' <$> get 
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping 
          $ Data.Map.Strict.insert 
              kitchenCabinetBelowShelfGID buildCabinet 
                $ (_unGIDToDataMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}})

{-

buildShelf :: Object 
buildShelf= Object { 
      _shortName' = "A shelf next to a sink"
    , _odescription' = [desc]
    , _descriptives' = []
    , _moveability' = NotMoveable
    , _perceptability' = Perceptible 
    , _mNexus' =  (Just . Nexus . Left) shelfContainer
  }

-}

buildCabinet :: Object 
buildCabinet = Object { 
      _shortName' = "A cabinet, under the shelf."
    , _odescription' = [desc]
    , _descriptives' = [] 
    , _moveability' = NotMoveable
    , _perceptability' = Perceptible
    , _orientation' = orientation
    , _mNexus' = (Just . Nexus . Left) cabinetContainer
  }
  where 
    desc = "You can put things in it."

orientation :: Orientation 
orientation = AnchoredTo' (kitchenShelfGID,PlacedUnder)
{-
data Orientation 
  = ContainedBy' ContainedBy 
  | Inventory 
  | Floor 
  | AnchoredTo' (GID Object, Proximity) 
  | Anchoring RoomAnchor
    deriving stock Show 
-}
cabinetContainer :: Containment
cabinetContainer = (Containment . This) containedIn

{-
containedIn :: ContainedIn
containedIn = ContainedIn 
  {_containerInterface' = ContainerInterface' containerInterface
  , _containedIn' = ContainerMap Data.Map.Strict.empty  
  }
-}

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
  