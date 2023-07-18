module HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.Shelf where
import HauntedHouse.Game.Object.Container.Domain (Containing
                                                  , RelatedObjects
                                                  , AttachedTo
                                                  , Moveable (NotMovable)
                                                  , Container (..))
import HauntedHouse.Game.Object.Domain (Object (..))

kitchenShelf :: Maybe AttachedTo -> Containing -> RelatedObjects -> Object
kitchenShelf containedBy containing relatedObjects = Object 
  { _container = Just kitchenShelfContainer 
  , _containedBy = containedBy 
  , _moveability = NotMovable 
  , _odescription = "A kitchen shelf to the right of the sink" 
  }
  where 
    kitchenShelfContainer = Container 
      { _isOpen = Just True 
      , _containing = containing 
      , _lockState = Nothing 
      , _relatedObjects = relatedObjects 
      }