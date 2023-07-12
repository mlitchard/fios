module HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkCabinets where
import HauntedHouse.Game.Object (Object (..))
import HauntedHouse.Game.Object.Container
import HauntedHouse.Game.GID (GID(..))
import Data.These (These(..))

kitchenSinkCabinetAbove :: Object
kitchenSinkCabinetAbove = Object
  { _container = kitchenSinkCabinetAboveContainerState
  , _containedBy = Nothing
  , _moveability = NotMovable
  , _odescription = "A cabinet above the sink"
  }

kitchenSinkCabinetAboveContainerState :: ContainerState
kitchenSinkCabinetAboveContainerState = ContainerState 
  (AttachedToObject (GID 0), Just (This kitchenCabinetAboveContainer))

{-
data Container = Container
  { _isOpen :: Maybe Bool
  , _cinv :: Maybe (NonEmpty (GID ObjectName))
  , _lockState :: Maybe LockState
  }
  -}
kitchenCabinetAboveContainer :: Container
kitchenCabinetAboveContainer = Container 
  { _isOpen = Just False
  , _cinv = Nothing
  , _lockState = Nothing
  }

kitchenSinkCabinetBelow :: Object
kitchenSinkCabinetBelow = Object
  { _container = kitchenSinkCabinetBelowContainerState
  , _containedBy = Nothing
  , _moveability = NotMovable
  , _odescription = "A cabinet below the sink"
  }

kitchenSinkCabinetBelowContainerState :: ContainerState
kitchenSinkCabinetBelowContainerState = ContainerState 
  (AttachedToObject (GID 0), Just (This kitchenCabinetBelowContainer))

kitchenCabinetBelowContainer :: Container
kitchenCabinetBelowContainer = Container 
  { _isOpen    = Just True
  , _cinv      = Nothing
  , _lockState = Nothing
  }