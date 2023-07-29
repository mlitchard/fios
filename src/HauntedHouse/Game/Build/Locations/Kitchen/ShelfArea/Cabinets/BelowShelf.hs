module HauntedHouse.Game.Build.Locations.Kitchen.ShelfArea.Cabinets.BelowShelf 
  (buildKitchenCabinetBelowShelf) where
import HauntedHouse.Game.Model (GameStateExceptT, GameState (..))
import HauntedHouse.Game.Model.Mapping (GIDToDataMapping (..))
import HauntedHouse.Game.Model.World
        (Object (..) , World (..), Container (..), ContainedBy (ByLocation), LockState (Unlocked))
import qualified Data.Map.Strict (insert, fromList)
import HauntedHouse.Game.Model.Object.Relation (RelatedObjects (..), Moveablility (NotMovable), Placeability (..), LeftOrRight (OnLeft))
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Build.ObjectTemplate (kitchenCabinetAboveSinkGID, kitchenCabinetBelowSinkGID)
import HauntedHouse.Game.Build.LocationTemplate (kitchenGID)

buildKitchenCabinetBelowShelf :: GameStateExceptT ()
buildKitchenCabinetBelowShelf = do
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
        ,(PlaceNextTo OnLeft, Just placeNextTo)]
    placeNextTo = Data.List.NonEmpty.fromList [kitchenCabinetBelowSinkGID]
