module HauntedHouse.Game.Object.Container where
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Agent.Domain (AgentName)
import HauntedHouse.Tokenizer (Lexeme)
import HauntedHouse.Game.Location.Domain (LocationName)


newtype ObjectName = ObjectName Lexeme deriving stock (Eq,Ord,Show)

instance ToText ObjectName where
    toText :: ObjectName -> Text
    toText (ObjectName oname) = toText oname
    
data ContainerState
    = ContainedIn ContainedIn 
    | Containing (Maybe Container) -- Nothing means not a container
    deriving stock Show

data ContainedIn 
    = ContainedInAgent (GID AgentName)
    | ContainedInObject (GID ObjectName)
    | ContainedInLocation (GID LocationName)
    deriving stock Show

data Container = Container {
    _isOpen :: Bool
  , _cinv :: [GID ObjectName]
  , _lockState :: Maybe LockState
} deriving stock Show

data Moveable = Moveable | NotMovable deriving stock (Eq,Ord,Enum,Show)

data LockState = Locked | Unlocked deriving stock (Show,Eq,Ord)