module HauntedHouse.Game.Build.Locations.Kitchen.KitchenSinkShelf.Shelf
        (buildKitchenShelf) where
import HauntedHouse.Game.Model
import HauntedHouse.Game.Model.Mapping (GIDToDataMapping (..))
import HauntedHouse.Game.Model.World 
import qualified Data.Map.Strict (insert, fromList)
import HauntedHouse.Game.Build.ObjectTemplate
import HauntedHouse.Game.Build.LocationTemplate (kitchenGID)
import HauntedHouse.Game.Model.Object.Relation
import qualified Data.List.NonEmpty

buildKitchenShelf :: GameStateExceptT ()
buildKitchenShelf =  do
  world <- _world' <$> get
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping $ Data.Map.Strict.insert kitchenShelfGID buildShelf
          $ (_unGIDMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}}) 

buildShelf :: Object 
buildShelf= Object 
  { _container' = Just buildShelfContainer 
  , _containedBy' = Just (ByLocation kitchenGID)
  , _moveability' = NotMovable 
  , _odescription' = "It's a shelf. You can put things on it"
  }

buildShelfContainer :: Container
buildShelfContainer = Container
  { _isOpen'          = Nothing
  , _lockState'       = Nothing 
  , _relatedObjects'  = relationToOtherObjects 
  }

relationToOtherObjects :: RelatedObjects Object 
relationToOtherObjects = 
  RelatedObjects $ Data.Map.Strict.fromList relatedObjects
  where 
    relatedObjects = 
      [(PlaceIn, Nothing)
        ,(PlaceUnder, Just placeUnder)
        ,(PlaceAbove, Just placeAbove)
        ,(PlaceNextTo OnLeft, Just placeNextTo)]
    placeAbove = Data.List.NonEmpty.fromList [kitchenCabinetAboveShelfGID]
    placeUnder = Data.List.NonEmpty.fromList [kitchenCabinetBelowShelfGID]
    placeNextTo = Data.List.NonEmpty.fromList [kitchenSinkGID]