module HauntedHouse.Build.Locations.Kitchen.SinkArea.Sink 
  where

import Data.List.NonEmpty qualified (fromList)
import Data.Map.Strict qualified

import HauntedHouse.Build.LocationTemplate (kitchenGID)
import HauntedHouse.Build.ObjectTemplate 
import HauntedHouse.Game.Model (GameStateExceptT, GameState (..))
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.Object.Relation 
import HauntedHouse.Game.Model.World
import Data.These (These(..))

buildKitchenSink :: GameStateExceptT ()
buildKitchenSink =  do
  world <- _world' <$> get
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = GIDToDataMapping $ Data.Map.Strict.insert kitchenSinkGID buildSink 
                $ (_unGIDToDataMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}}) 

buildSink :: Object 
buildSink = Object
  {_shortName' = "The kitchen sink"
  , _related' = related 
  , _moveability' = NotMoveable 
  , _odescription' = "This sink doesn't work. You can put small objects in it though."
  }

sinkContainer :: Container 
sinkContainer = Container 
  { _containerInterFace' = sinkInterface 
  , _contained' = This sinkObjects  
  }


sinkInterface :: Interface Container 
sinkInterface = Interface { _openState'  = Nothing }

sinkObjects :: ContainedIn
sinkObjects = ContainedIn $ ContainerMap  Data.Map.Strict.empty  

related :: Relations 
related = Relations 
  { _position'    = AnchoredByRoom kitchenGID 
  , _neighbors'   = neighbors
  }

neighbors :: NeighborMap Object Proximity
neighbors = NeighborMap $ Data.Map.Strict.fromList 
  [(kitchenCabinetAboveSinkGID,PlacedAbove)]

