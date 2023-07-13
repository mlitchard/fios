module HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.Cabinets 
  where
import Data.These (These (..))
import HauntedHouse.Game.GID
import HauntedHouse.Game.Object
import HauntedHouse.Game.Object (AttachedTo(AttachedToObject))

type ContainedBy = GID ObjectName
type Containing  = [GID ObjectName]

kitchenCabinetAboveShelf :: ContainedBy -> Containing -> Object
kitchenCabinetAboveShelf containedBy containing = Object
  { _container = Just kitchenCabinetAboveShelfContainerState
  , _containedBy = Just (AttachedToObject containedBy)
  , _moveability = NotMovable
  , _odescription = "A cabinet above the kitchen shelf"
  }
  where
    kitchenCabinetAboveShelfContainerState :: ContainerState
    kitchenCabinetAboveShelfContainerState = ContainerState 
      (This kitchenCabinetAboveShelfContainer)

    kitchenCabinetAboveShelfContainer :: Container
    kitchenCabinetAboveShelfContainer = Container 
      { _isOpen = Just False
      , _cinv = containing 
      , _lockState = Nothing
      }

kitchenCabinetBelowShelf :: ContainedBy -> Containing -> Object
kitchenCabinetBelowShelf containedBy containing = Object
  { _container = Just kitchenCabinetBelowShelfContainerState
  , _containedBy = Just (AttachedToObject containedBy) 
  , _moveability = NotMovable
  , _odescription = "A cabinet below the kitchen shelf"
  }
  where
    kitchenCabinetBelowShelfContainerState :: ContainerState
    kitchenCabinetBelowShelfContainerState = ContainerState 
      (This kitchenCabinetBelowShelfContainer)

    kitchenCabinetBelowShelfContainer :: Container
    kitchenCabinetBelowShelfContainer = Container 
      { _isOpen = Just False
      , _cinv = containing
      , _lockState = Nothing
      }


