module HauntedHouse.Game.World where
import HauntedHouse.Game.Object
import HauntedHouse.Game.Object.Container (ObjectLabel)

{-
data GameState = GameState
  { _objectMap :: ObjectMap
  , _locationMap :: LocationMap
  , _agentMap :: AgentMap
  , _report :: [Text]
  , _player :: GID AgentLabel
  , _narration :: Narration
  , _newScene :: Bool
  , _clarification :: Maybe (NonEmpty Text)
  }
  deriving stock (Show)
-}

{-
cabinetON :: ObjectLabel
cabinetON = ObjectLabel CABINET
-}
{-
data Object = Object
  { _container :: ContainerState
  , _contained :: Maybe ObjectLabel
  , _moveability :: Moveable
  , _odescription :: Text
  }
-}
{-
data Container = Container
  { _isOpen :: Bool
  , _cinv :: Maybe (NonEmpty (GID ObjectLabel))
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
plantON :: ObjectLabel
plantON = ObjectLabel PLANT 

potON :: ObjectLabel 
potON = ObjectLabel POT 

soil :: ObjectLabel
soil = ObjectLabel BAG 

can :: ObjectLabel
can = ObjectLabel CAN 
-}