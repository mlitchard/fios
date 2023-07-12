module HauntedHouse.Game.World where
import HauntedHouse.Game.Object
import HauntedHouse.Game.Object.Container (ObjectName)

{-
data GameState = GameState
  { _objectMap :: ObjectMap
  , _locationMap :: LocationMap
  , _agentMap :: AgentMap
  , _report :: [Text]
  , _player :: GID AgentName
  , _narration :: Narration
  , _newScene :: Bool
  , _clarification :: Maybe (NonEmpty Text)
  }
  deriving stock (Show)
-}

{-
cabinetON :: ObjectName
cabinetON = ObjectName CABINET
-}
{-
data Object = Object
  { _container :: ContainerState
  , _contained :: Maybe ObjectName
  , _moveability :: Moveable
  , _odescription :: Text
  }
-}
{-
data Container = Container
  { _isOpen :: Bool
  , _cinv :: Maybe (NonEmpty (GID ObjectName))
  , _lockState :: Maybe LockState
  }
-}
{-
sinkCabinet :: Object 
sinkCabinet = Object 
  { _container =  

  }
-}
{-
plantON :: ObjectName
plantON = ObjectName PLANT 

potON :: ObjectName 
potON = ObjectName POT 

soil :: ObjectName
soil = ObjectName BAG 

can :: ObjectName
can = ObjectName CAN 
-}