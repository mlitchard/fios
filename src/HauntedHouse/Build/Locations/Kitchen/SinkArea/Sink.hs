module HauntedHouse.Build.Locations.Kitchen.SinkArea.Sink 
  where

import Data.List.NonEmpty qualified (fromList)
import Data.Map.Strict qualified
import Data.These qualified (These(..))

import HauntedHouse.Build.DescriptiveTemplate
import HauntedHouse.Build.LocationTemplate (kitchenGID)
import HauntedHouse.Build.ObjectTemplate 
import HauntedHouse.Game.Model (GameStateExceptT, GameState (..))
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.World

buildKitchenSink :: GameStateExceptT ()
buildKitchenSink = do
  world <- _world' <$> get
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping $ Data.Map.Strict.insert kitchenSinkGID buildSink 
          $ (_unGIDToDataMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}}) 

buildSink :: Object 
buildSink = Object 
  { _shortName' = "A kitchen sink."
  , _moveability' = NotMoveable
  , _containment' = (Just . Left) sinkContainer
  , _odescription' = "This sink is broken. You can put things in it."
  , _descriptors' = [kitchenLabel]
  }

sinkContainer :: Container 
sinkContainer = (Container . Data.These.That) containedOn 

containedOn :: ContainedOn 
containedOn = (ContainedOn . ContainerMap) Data.Map.Strict.empty