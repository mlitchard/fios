module HauntedHouse.Build.Locations.Kitchen.SinkArea.Cabinets.AboveSink where

import HauntedHouse.Build.DescriptiveTemplate
    ( unlockedLabel, kitchenLabel, visibleLabel ) 
import HauntedHouse.Game.Model.Mapping 
        (GIDToDataMapping (..), ContainerMap (..))
import HauntedHouse.Game.Model.World
import qualified Data.Map.Strict
import HauntedHouse.Build.ObjectTemplate (kitchenCabinetAboveSinkGID)
import Data.These (These(..))
import HauntedHouse.Game.Model.Condition (Moveability(..), Perceptibility (..))

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
      _shortName' = "A cabinet, above the sink."
    , _odescription' = [desc]
    , _descriptives' = []
    , _moveability' = NotMoveable
    , _perceptability' = Perceptible
    , _mNexus' =  (Just . Nexus . Left) cabinetContainer
  }
  where 
    desc = "You can put things in it."

cabinetContainer :: Containment
cabinetContainer = (Containment . This) containedIn

{-

data ContainedIn = ContainedIn
  { _containerInterface'  :: Interface
  , _containedIn'         :: ContainerMap Object
  } 

-}

containedIn :: ContainedIn
containedIn = ContainedIn 
  {_containerInterface' = ContainerInterface' containerInterface
  , _containedIn' = ContainerMap Data.Map.Strict.empty  
  }

{-

data ContainerInterface = ContainerInterface {
      _openState'    :: OpenState
    , _openAction'   :: GameStateExceptT ()
    , _closeAction'  :: GameStateExceptT ()
    , _lockAction'   :: GameStateExceptT ()
    , _unlockAction' :: GameStateExceptT ()
  }

-}

containerInterface :: ContainerInterface 
containerInterface = ContainerInterface {
      _openState'     = Open 
    , _openAction'    = pass 
    , _closeAction'   = pass 
    , _lockAction'    = pass
    , _unlockAction'  = pass
  }
