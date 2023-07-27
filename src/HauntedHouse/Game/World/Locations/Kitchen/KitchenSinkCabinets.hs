module HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkCabinets where

import HauntedHouse.Game.Object (Object (..))
import HauntedHouse.Game.Object.Container.Domain (AttachedTo
                                                  , Containing
                                                  , RelatedObjects
                                                  , Container (..)
                                                  , Moveable (..))
{-
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
      , _containing = containing 
      , _lockState = Nothing 
      , _relatedObjects = relatedObjects 
      }

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
      , _relatedObjects = relatedObjects 
      }
-}