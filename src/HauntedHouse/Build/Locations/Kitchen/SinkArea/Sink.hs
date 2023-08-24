module HauntedHouse.Build.Locations.Kitchen.SinkArea.Sink 
  where

import Data.List.NonEmpty qualified (fromList)
import Data.Map.Strict qualified
import Data.These qualified (These(..))

import HauntedHouse.Build.DescriptiveTemplate
import HauntedHouse.Build.LocationTemplate (kitchenGID)
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
    _shortName'     = "A kitchen sink."
  , _odescription'  = [desc]
  , _descriptives' = []
  , _moveability'   = NotMoveable
  , _perceptability' = Perceptible
  , _mNexus'         = (Just . Nexus . Left) sinkContainer 
  }
  where 
    desc = "This sink is broken. You can put things in it."

{-

newtype Containment = Containment
  { _unContainment' :: These ContainedIn
                        (Either ContainedOn ContainedBy)
  } deriving stock (Show)

-}
sinkContainer :: Containment 
sinkContainer = (Containment . Data.These.That) (Left containedOn) 

containedOn :: ContainedOn 
containedOn = (ContainedOn . ContainerMap) Data.Map.Strict.empty
