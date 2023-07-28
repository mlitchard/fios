module HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.Shelf where

{-
kitchenShelf :: Maybe AttachedTo 
                  -> Maybe Containing 
                  -> RelatedObjects 
                  -> Object
kitchenShelf containedBy containing relatedObjects = Object 
  { _container'   = Just kitchenShelfContainer 
  , _containedBy'   = containedBy 
  , _moveability'   = NotMovable 
  , _odescription'  = "A kitchen shelf to the right of the sink" 
  }
  where 
    kitchenShelfContainer = Container 
      { _isOpen = Just True 
      , _containing = containing 
      , _lockState = Nothing 
      , _relatedObjects = relatedObjects 
      }
-}