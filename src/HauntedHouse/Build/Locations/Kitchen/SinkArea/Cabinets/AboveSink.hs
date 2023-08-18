module HauntedHouse.Build.Locations.Kitchen.SinkArea.Cabinets.AboveSink where

import HauntedHouse.Build.DescriptiveTemplate
    ( unlockedLabel, kitchenLabel, visibleLabel )  
import HauntedHouse.Game.Model (GameStateExceptT, GameState (..))
import HauntedHouse.Game.Model.Mapping (GIDToDataMapping (..), ContainerMap (..))
import HauntedHouse.Game.Model.World
        (Object (..), World (..), Containment (..), ContainedIn (..), Moveability (..), Interface (..))
import qualified Data.Map.Strict
import HauntedHouse.Build.ObjectTemplate (kitchenCabinetAboveSinkGID)
import Data.These (These(..))

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
buildCabinet = Object 
  { _shortName' = "A cabinet, above the sink."
  , _moveability' = NotMoveable
  , _containment' = (Just . Left) cabinetContainer
  , _odescription' = "You can put things in it."
  , _conditions'  = [kitchenLabel,unlockedLabel,visibleLabel]}

cabinetContainer :: Containment
cabinetContainer = (Containment . This) containedIn

containedIn :: ContainedIn
containedIn = ContainedIn 
  {_interface' = Open
  , _containedIn' = ContainerMap Data.Map.Strict.empty  
  }