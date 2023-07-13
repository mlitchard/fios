module HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkCabinets where
import HauntedHouse.Tokenizer
import HauntedHouse.Game.Object (Object (..))
import HauntedHouse.Game.Object.Container
import HauntedHouse.Game.GID (GID(..))
import Data.These (These(..))

type ContainedBy = GID ObjectName
type Containing  = [GID ObjectName]

kitchenSinkCabinetAboveName :: ObjectName
kitchenSinkCabinetAboveName = ObjectName CABINET

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

{-
data Container = Container
  { _isOpen :: Maybe Bool
  , _cinv :: Maybe (NonEmpty (GID ObjectName))
  , _lockState :: Maybe LockState
  }
  -}

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