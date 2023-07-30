module HauntedHouse.Build.Locations.Kitchen.Sink.Cabinets.AboveSink 
  where (kitchenSinkCabinetAbove)

kitchenSinkCabinetAbove :: Maybe AttachedTo 
                              -> Maybe Containing 
                              -> RelatedObjects 
                              -> Object
kitchenSinkCabinetAbove containedBy containing relatedObjects = Object
  { _container = Just kitchenSinkCabinetAboveContainer
  , _containedBy = containedBy
  , _moveability = NotMovable
  , _odescription = "A cabinet above the sink"
  }
  where
    kitchenSinkCabinetAboveContainer :: Container
    kitchenSinkCabinetAboveContainer = Container 
      { _isOpen = Just True 
      , _lockState = Nothing 
      , _relatedObjects = relationToOtherObjects 
      }

relationToOtherObjects :: RelatedObjects Object 
relationToOtherObjects = 
  RelatedObjects $ Data.Map.Strict.fromList relatedObjects
  where 
    relatedObjects = 
      [(PlaceIn, Nothing)
        ,(PlaceUnder, Just placeUnder)
        ,(PlaceNextTo OnRight, Just placeNextTo)]
    placeUnder = Data.List.NonEmpty.fromList [kitchenSinkGID]
    placeNextTo = Data.List.NonEmpty.fromList [kitchenCabinetAboveShelfGID]