module HauntedHouse.Game.Build.Locations.Kitchen.KitchenSinkShelf.Cabinets
  where
    
{-
kitchenCabinetAboveShelf :: Maybe AttachedTo 
                              -> Maybe Containing 
                              -> RelatedObjects 
                              -> Object
kitchenCabinetAboveShelf containedBy containing relatedObjects = Object
  { _container = Just kitchenCabinetAboveShelfContainer
  , _containedBy = containedBy
  , _moveability = NotMovable
  , _odescription = "A cabinet above the kitchen shelf"
  }
  where
    kitchenCabinetAboveShelfContainer :: Container
    kitchenCabinetAboveShelfContainer = Container
      { _isOpen = Nothing 
      , _containing = containing
      , _lockState = Just Unlocked
      , _relatedObjects = relatedObjects 
      }

kitchenCabinetBelowShelf :: Maybe AttachedTo 
                              -> Maybe Containing
                              -> RelatedObjects 
                              -> Object
kitchenCabinetBelowShelf containedBy containing relatedObjects = Object
  { _container = Just kitchenCabinetBelowShelfContainer 
  , _containedBy = containedBy
  , _moveability = NotMovable
  , _odescription = "A cabinet below the kitchen shelf"
  }
  where
    kitchenCabinetBelowShelfContainer :: Container
    kitchenCabinetBelowShelfContainer = Container
      { _isOpen = Just True
      , _containing = containing
      , _lockState = Just Unlocked
      , _relatedObjects = relatedObjects}
-}