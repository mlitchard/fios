module HauntedHouse.Build.Locations.Kitchen.SinkArea.Cabinets.BelowSink where

import HauntedHouse.Build.DescriptiveTemplate
    ( unlockedLabel, kitchenLabel, visibleLabel )  

import HauntedHouse.Game.Model.Mapping (GIDToDataMapping (..), ContainerMap (..))
import HauntedHouse.Game.Model.World
    
import qualified Data.Map.Strict
import HauntedHouse.Build.ObjectTemplate (kitchenCabinetBelowSinkGID, kitchenSinkGID)
import Data.These (These(..))
import HauntedHouse.Game.Model.Condition (Moveability(..), Perceptibility (..), Proximity (PlacedUnder))

buildKitchenCabinetBelowSink :: GameStateExceptT ()
buildKitchenCabinetBelowSink = do
  world <- _world' <$> get 
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping 
          $ Data.Map.Strict.insert 
              kitchenCabinetBelowSinkGID buildCabinet 
                $ (_unGIDToDataMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}})


{-
data Object = Object {
    _shortName'       :: Text
  , _odescription'    :: [Text]
  , _descriptives'    :: [Label Adjective]
  , _moveability'     :: Moveability
  , _perceptability'  :: Perceptibility
  , _mNexus'          :: Maybe Nexus
}
-}

buildCabinet :: Object 
buildCabinet = Object { 
      _shortName' = "cabinet"
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
orientation = AnchoredTo' (kitchenSinkGID, PlacedUnder)

cabinetContainer :: Containment
cabinetContainer = (Containment . This) containedIn

containedIn :: ContainedIn
containedIn = ContainedIn { 
      _containerInterface' = ContainerInterface' containerInterface
    , _containedIn' = ContainerMap Data.Map.Strict.empty  
  }
  
containerInterface :: ContainerInterface
containerInterface = ContainerInterface {
      _openState'     = Closed 
    , _describe' = mempty
    , _openAction'    = print ("This is where open happens" :: Text) 
    , _closeAction'   = pass 
    , _lockAction'    = pass 
    , _unlockAction'  = pass
  }   