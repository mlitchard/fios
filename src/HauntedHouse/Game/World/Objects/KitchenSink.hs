module HauntedHouse.Game.World.Objects.KitchenSink where

import Data.These (These (..))
import HauntedHouse.Game.Object 
import HauntedHouse.Game.Object.Container (ContainerState, Shelf (..))
import HauntedHouse.Game.GID (GID(..))

{-
data Object = Object
  { _container :: ContainerState 
  , _contained :: Maybe ObjectName
  , _moveability :: Moveable
  , _odescription :: Text
  }
-}
kitchenSink :: Object
kitchenSink = Object 
  { _container = kitchenSinkContainerState 
  , _containedBy = Nothing 
  , _moveability = NotMovable 
  , _odescription = 
      "A kitchen sink."
        <> " There is a cabinet above the sink."
        <> " There is a cabinet below the sink"
        <> " There is a shelf to the right of the sink."
  }

-- ContainerState (AttachedTo, Maybe (These Container Shelf))
kitchenSinkContainerState :: ContainerState 
kitchenSinkContainerState = ContainerState 
  (AttachedToLocation (GID 0), Just (These kitchenSinkAsContainer kitchenSinkAsShelf))

{-
{ _isOpen :: Maybe Bool
  , _cinv :: Maybe (NonEmpty (GID ObjectName))
  , _lockState :: Maybe LockState
  }
-}
kitchenSinkAsContainer :: Container 
kitchenSinkAsContainer = Container 
  { _isOpen = Nothing -- Can't open or close a sink 
  , _cinv = Nothing
  , _lockState = Nothing -- Can't lock a sink
  }

{-

{ placeablity :: These PlaceUnder PlaceAbove
  , _sinv :: Maybe (NonEmpty (GID ObjectName, These (These PlaceUnder PlaceOn) PlaceAbove))
  } deriving stock Show

-}
kitchenSinkAsShelf :: Shelf 
kitchenSinkAsShelf = Shelf
  { _placeability = This PlaceOn   
  , _sinv = Nothing 
  }


