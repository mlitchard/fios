module HauntedHouse.Game.World.Objects.KitchenSinkShelf.Cabinets 
  where
import Data.These (These (..))
import HauntedHouse.Game.GID
import HauntedHouse.Game.Object

kitchenShelfCabinetAbove :: Object
kitchenShelfCabinetAbove = Object
  { _container = kitchenShelfCabinetAboveContainerState
  , _containedBy = Nothing
  , _moveability = NotMovable
  , _odescription = "A cabinet above the kitchen shelf"
  }

kitchenShelfCabinetAboveContainerState :: ContainerState
kitchenShelfCabinetAboveContainerState = ContainerState 
  (AttachedToObject (GID 0), Just (This kitchenCabinetAboveShelfContainer))

kitchenCabinetAboveShelfContainer :: Container
kitchenCabinetAboveShelfContainer = Container 
  { _isOpen = Just False
  , _cinv = Nothing
  , _lockState = Nothing
  }