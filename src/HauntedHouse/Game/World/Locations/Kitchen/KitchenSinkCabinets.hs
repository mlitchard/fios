module HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkCabinets where
import HauntedHouse.Tokenizer
import HauntedHouse.Game.Object (Object (..))
import HauntedHouse.Game.Object.Container
import Data.These (These(..))
import HauntedHouse.Game.World.Objects
import HauntedHouse.Game.Object.Atomic (ObjectLabel (..))

kitchenSinkCabinetAboveLabel :: ObjectLabel
kitchenSinkCabinetAboveLabel = ObjectLabel CABINET

kitchenSinkCabinetAbove :: ContainedBy -> Containing -> Object
kitchenSinkCabinetAbove containedBy containing = Object
  { _container = Just kitchenSinkCabinetAboveContainerState
  , _containedBy = Just (AttachedToObject containedBy)
  , _moveability = NotMovable
  , _odescription = "A cabinet above the sink"
  }
  where
    kitchenSinkCabinetAboveContainerState :: ContainerState
    kitchenSinkCabinetAboveContainerState = ContainerState 
      (This kitchenCabinetAboveContainer)

    kitchenCabinetAboveContainer :: Container
    kitchenCabinetAboveContainer = Container 
      { _isOpen = Just False
      , _cinv = containing 
      , _lockState = Nothing
      }


kitchenSinkCabinetBelowLabel :: ObjectLabel
kitchenSinkCabinetBelowLabel = ObjectLabel CABINET   

kitchenSinkCabinetBelow :: ContainedBy -> Containing -> Object
kitchenSinkCabinetBelow containedBy containing = Object
  { _container = Just kitchenSinkCabinetBelowContainerState
  , _containedBy = Just (AttachedToObject containedBy) 
  , _moveability = NotMovable
  , _odescription = "A cabinet below the sink"
  }
  where
    kitchenSinkCabinetBelowContainerState :: ContainerState
    kitchenSinkCabinetBelowContainerState = ContainerState 
      (This kitchenCabinetBelowContainer)

    kitchenCabinetBelowContainer :: Container
    kitchenCabinetBelowContainer = Container 
      { _isOpen    = Just True
      , _cinv      = containing 
      , _lockState = Nothing
      }