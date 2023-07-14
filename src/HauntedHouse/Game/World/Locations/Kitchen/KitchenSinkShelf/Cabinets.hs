module HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.Cabinets
  where
import Data.These (These (..))
import HauntedHouse.Game.GID
import HauntedHouse.Game.Object
import HauntedHouse.Tokenizer (Lexeme(CABINET))
import HauntedHouse.Game.World.Objects
import HauntedHouse.Game.Object.Atomic (ObjectLabel (..))


kitchenCabinetAboveShelfLabel :: ObjectLabel
kitchenCabinetAboveShelfLabel = ObjectLabel CABINET 

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

kitchenCabinetBelowShelfLabel :: ObjectLabel
kitchenCabinetBelowShelfLabel = ObjectLabel CABINET 

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


