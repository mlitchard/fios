module HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkCabinet where
import HauntedHouse.Game.Object (Object (..))
import HauntedHouse.Game.Object.Container
import HauntedHouse.Game.GID (GID(..))
import Data.These (These(..))

kitchenSinkCabinet :: Object
kitchenSinkCabinet = Object
  { _container = kitchenSinkCabinetContainerState
  , _containedBy = Nothing
  , _moveability = NotMovable
  , _odescription = "A cabinet below the sink"
  }

kitchenSinkCabinetContainerState :: ContainerState
kitchenSinkCabinetContainerState = ContainerState 
  (AttachedToObject (GID 0), Just (This kitchenCabinetContainer))

{-
data Container = Container
  { _isOpen :: Maybe Bool
  , _cinv :: Maybe (NonEmpty (GID ObjectName))
  , _lockState :: Maybe LockState
  }
  -}
kitchenCabinetContainer :: Container
kitchenCabinetContainer = Container 
  { _isOpen = Just False
  , _cinv = Nothing
  , _lockState = Nothing
  }