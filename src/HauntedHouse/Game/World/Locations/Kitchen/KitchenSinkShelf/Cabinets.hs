module HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.Cabinets
  where

import HauntedHouse.Game.Object
import HauntedHouse.Game.Object.Container.Domain ( Container(..)
                                                  , RelatedObjects
                                                  , Containing
                                                  , AttachedTo (..)
                                                  , Moveable (..), LockState (Unlocked))

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