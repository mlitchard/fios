module HauntedHouse.Build.Locations.Kitchen.Sink.Cabinets.BelowSink
  where (kitchenSinkCabinetBelow)

kitchenSinkCabinetBelow :: Maybe AttachedTo 
                            -> Maybe Containing 
                            -> RelatedObjects 
                            -> Object
kitchenSinkCabinetBelow containedBy containing relatedObjects = Object
  { _container = Just kitchenSinkCabinetBelowContainer
  , _containedBy = containedBy 
  , _moveability = NotMovable
  , _odescription = "A cabinet below the sink"
  }
  where
    kitchenSinkCabinetBelowContainer :: Container
    kitchenSinkCabinetBelowContainer = Container 
      { _isOpen = Just True 
      , _containing = containing 
      , _lockState = Nothing 
      , _relatedObjects = relationToOtherObjects 
      }

relationToOtherObjects :: RelatedObjects Object 
relationToOtherObjects = 
  RelatedObjects $ Data.Map.Strict.fromList relatedObjects
  where 
    relatedObjects = 
      [(PlaceIn, Nothing)
        ,(PlaceAbove, Just placeAbove)
        ,(PlaceNextTo OnRight, Just placeNextTo)]
    placeAbove =  Data.List.NonEmpty.fromList [kitchenSink]
    placeNextTo = Data.List.NonEmpty.fromList [kitchenCabinetBelowShelfGID]