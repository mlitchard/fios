module HauntedHouse.Build.Locations.Kitchen.SinkArea.Sink 
  where

import Data.Map.Strict qualified
import Data.These qualified (These(..))

import HauntedHouse.Build.ObjectTemplate 
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Model.Condition (Perceptibility(Perceptible), Moveability (..))

buildKitchenSink :: GameStateExceptT ()
buildKitchenSink = do
  world <- _world' <$> get
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping $ Data.Map.Strict.insert kitchenSinkGID buildSink 
          $ (_unGIDToDataMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}}) 

buildSink :: Object 
buildSink = Object { 
    _shortName'     = "kitchen sink."
  , _odescription'  = [desc]
  , _descriptives' = []
  , _moveability'   = NotMoveable
  , _perceptability' = Perceptible
  , _orientation' = orientation
  , _mNexus'         = (Just . Containment') sinkContainer 
  }
  where 
    desc = "This sink is broken. You can put things in it."

orientation :: Orientation 
orientation = Anchoring EastAnchor

sinkContainer :: Containment 
sinkContainer = (Containment . Data.These.That) containedOn 

containedOn :: ContainedOn 
containedOn = (ContainedOn . ContainerMap) Data.Map.Strict.empty
