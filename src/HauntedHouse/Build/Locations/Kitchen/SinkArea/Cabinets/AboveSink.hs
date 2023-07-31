module HauntedHouse.Build.Locations.Kitchen.Sink.Cabinets.AboveSink
  where (kitchenSinkCabinetAbove)
{-
buildKitchenCabinetAboveSink :: GameStateExceptT ()
buildKitchenCabinetAboveSink = do
  world <- _world' <$> get 
  let objectMap' :: GIDToDataMapping Object 
      objectMap' = 
        GIDToDataMapping 
          $ Data.Map.Strict.insert 
              kitchenCabinetAboveSinkGID buildCabinet 
                $ (_unGIDMapping' . _objectMap') world
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}})
-}
{-

data Object = Object
  { _related          :: RelatedObjects
  , _containedBy'     :: Maybe ContainedBy
  , _moveability'     :: Moveablility
  , _odescription'    :: Text
  } deriving stock Show

-}
buildCabinet :: Object 
buildCabinet = Object 
  { _related' = otherObjects 
  , _containedBy' = Just (ByObject kitchenSinkGID)
  , _moveability' = NotMovable 
  , _odescription' = "It's a cabinet. You can put things in it"
  }

otherObjects 
{-

-}
relationToOtherObjects :: RelatedObjects Object 
relationToOtherObjects = 
  RelatedObjects $ Data.Map.Strict.fromList relatedObjects
  where 
    relatedObjects = 
      [(PlaceIn, Nothing)
        ,(PlaceUnder, Just placeUnder)
        ,(PlaceNextTo OnLeft, Just placeNextTo)]
    placeUnder = Data.List.NonEmpty.fromList [kitchenSinkGID]
    placeNextTo = Data.List.NonEmpty.fromList [kitchenCabinetAboveSinkGID]