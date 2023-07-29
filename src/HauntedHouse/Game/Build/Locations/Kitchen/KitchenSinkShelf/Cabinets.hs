module HauntedHouse.Game.Build.Locations.Kitchen.KitchenSinkShelf.Cabinets
  where
import HauntedHouse.Game.Model (GameStateExceptT, GameState (..))
import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Model.Mapping
import qualified Data.Map.Strict
import HauntedHouse.Game.Build.ObjectTemplate
        (kitchenCabinetAboveSinkGID, kitchenShelfGID)
import HauntedHouse.Game.Model.Object.Relation
        (Moveablility(..), RelatedObjects (..), Placeability (..)
        , LeftOrRight (OnLeft))
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Build.LocationTemplate (kitchenGID)
buildKitchenCabinetAboveShelf :: GameStateExceptT ()
buildKitchenCabinetAboveShelf = do
  world <- _world' <$> get 
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping 
          $ Data.Map.Strict.insert 
              kitchenCabinetAboveSinkGID buildCabinet 
                $ (_unGIDMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}})

buildCabinet :: Object 
buildCabinet = Object 
  { _container' = Just buildCabinetContainer 
  , _containedBy' = Just (ByLocation kitchenGID)
  , _moveability' = NotMovable 
  , _odescription' = "It's a cabinet. You can put things in it"
  }

buildCabinetContainer :: Container
buildCabinetContainer = Container
  { _isOpen'          = Just False
  , _lockState'       = Just Unlocked 
  , _relatedObjects'  = relationToOtherObjects 
  }

relationToOtherObjects :: RelatedObjects Object 
relationToOtherObjects = 
  RelatedObjects $ Data.Map.Strict.fromList relatedObjects
  where 
    relatedObjects = 
      [(PlaceIn, Nothing)
        ,(PlaceUnder, Just placeUnder)
        ,(PlaceNextTo OnLeft, Just placeNextTo)]
    placeUnder = Data.List.NonEmpty.fromList [kitchenShelfGID]
    placeNextTo = Data.List.NonEmpty.fromList [kitchenCabinetAboveSinkGID]