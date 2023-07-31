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
{-
buildKitchenSink :: GameStateExceptT ()
buildKitchenSink =  do
  world <- _world' <$> get
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = GIDToDataMapping $ Data.Map.Strict.insert kitchenSinkGID buildSink 
                $ (_unGIDMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}}) 

buildSink :: Object 
buildSink = Object 
  { _container' = Just buildSinkContainer 
  , _containedBy' = Just (ByLocation kitchenGID)
  , _moveability' = NotMovable 
  , _odescription' = "It's a sink. Don't try and turn it on yet"
  }

  
{-
data Placeability
  = PlaceOn
  | PlaceUnder
  | PlaceAbove
  | PlaceIn
      deriving stock (Eq,Ord,Show)
-}
{-
relationToOtherObjects :: RelatedObjects Object 
relationToOtherObjects = 
  RelatedObjects $ Data.Map.Strict.fromList relatedObjects
  where 
    relatedObjects = 
      [(PlaceIn, Nothing)
        ,(PlaceUnder, Just placeUnder)
        ,(PlaceAbove, Just placeAbove)
        ,(PlaceNextTo OnRight, Just placeNextTo)]
    placeAbove = Data.List.NonEmpty.fromList [kitchenCabinetAboveSinkGID]
    placeUnder = Data.List.NonEmpty.fromList [kitchenCabinetBelowSinkGID]
    placeNextTo = Data.List.NonEmpty.fromList [kitchenShelfGID]
    -}
{-
-- a is Object
newtype RelatedObjects a
          = RelatedObjects (Data.Map.Strict.Map Placeability [GID a]) 
              deriving stock Show
 
-}
-}