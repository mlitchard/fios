module HauntedHouse.Game.Model.World where

import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Recognizer (Adjective)
import HauntedHouse.Game.Model.Condition (Inventory, Proximity, Moveability, Perceptibility (..))
import System.Console.Haskeline (InputT)
import Text.Show (Show(..))
import Prelude hiding (show)
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Model.GID (GID)
import Data.These (These)
import qualified Data.Map.Strict
import qualified Data.Text
import Control.Monad.Except (MonadError(throwError))

type GameStateExceptT = ExceptT Text (StateT GameState IO)

type InputGameStateExceptT = InputT GameStateExceptT

newtype AnchoredTo = AnchoredTo
  { _unAnchoredTo' :: Data.Map.Strict.Map (GID Object) (GID Object,Proximity)}
    deriving stock (Show, Eq, Ord)

newtype Containment = Containment
  { _unContainment' :: These ContainedIn
                        (Either ContainedOn ContainedBy)
  } deriving stock (Show)

data ContainedBy = ContainedBy
  { _containedBy' :: Either Inventory (GID Object)
  , _objectContained' :: GID Object
  } deriving stock (Show)

data ContainedIn = ContainedIn
  { _containerInterface'  :: Interface
  , _containedIn'         :: ContainerMap Object
  }

newtype ContainedOn = ContainedOn {_unContainedOn' :: ContainerMap Object}
  deriving stock (Eq,Ord,Show)

data ContainerInterface = ContainerInterface {
      _openState'    :: OpenState
    , _openAction'   :: GameStateExceptT ()
    , _closeAction'  :: GameStateExceptT ()
    , _lockAction'   :: GameStateExceptT ()
    , _unlockAction' :: GameStateExceptT ()
  }

instance Show ContainerInterface where
  show containerInterface = show (_openState' containerInterface)

newtype Exit = Exit { _toDestination' :: GID Location} deriving stock Show

newtype ExitGIDDataMap = ExitGIDDataMap {
  _unExitGIDDataMap' :: GIDToDataMapping Exit
  } deriving stock Show

newtype ExitGIDMap
  = ExitGIDMap {_unExitGIDMap' :: LabelToGIDMapping Exit Object}
      deriving stock Show

data GameState = GameState
  { _world'         :: World
  , _report'        :: [Text]
  , _player'        :: Player
  , _narration'     :: Narration
  , _verbosity'     :: Verbosity
  , _clarification' :: Maybe (NonEmpty Text)
  }

data Interface
  = ContainerInterface' ContainerInterface
  | PortalInterface
      deriving stock Show

data Location = Location {
  _title'             :: Text
  , _description'     :: Text
  , _anchoredObjects' :: RoomAnchors
  , _anchoredTo'      :: AnchoredTo
  , _floorInventory'  :: Maybe (Data.List.NonEmpty.NonEmpty (GID Object))
  , _objectLabelMap'  :: LabelToGIDListMapping Object Object
  , _directions'      :: Maybe ExitGIDMap
}

newtype Neighbors = Neighbors
  {_unNeighbors' :: NeighborMap Proximity Object} deriving stock Show

newtype Nexus = Nexus {
    _unNexus' :: Either Containment Portal
  } deriving stock (Show)

data Narration = Narration
  {_playerAction' :: Data.List.NonEmpty.NonEmpty Text
  ,_enviroment'   :: Data.List.NonEmpty.NonEmpty Text
  , _npcResponse' :: Data.List.NonEmpty.NonEmpty Text
  , _scene'       :: Scene
  } deriving stock Show

data Object = Object {
    _shortName'       :: Text
  , _odescription'    :: [Text]
  , _descriptives'    :: [Label Adjective]
  , _moveability'     :: Moveability
  , _perceptability'  :: Perceptibility
  , _mNexus'          :: Maybe Nexus
}

newtype ObjectAnchors = ObjectAnchors {
  _unObjectAnchors :: Data.Map.Strict.Map (GID Object) Neighbors
  } deriving stock Show

data OpenState = Open | Closed Lockability deriving stock Show

data Lockability = Locked | UnLocked | NotLockable deriving stock Show

data Player = Player
  { _playerLocation'  :: GID Location
  , _p_inv'           :: Maybe (Data.List.NonEmpty.NonEmpty (GID Object))
  } deriving stock Show

data Portal = Portal {
      _portalInterface' :: Interface
    , _portalExit' :: GID Exit
  } deriving stock Show

data RoomAnchor
  = NorthAnchor
  | SouthAnchor
  | WestAnchor
  | EastAnchor
  | NorthWestAnchor
  | NorthEastAnchor
  | SouthWestAnchor
  | SouthEastAnchor
  | CenterAnchor
    deriving stock (Show,Eq,Ord)

newtype RoomAnchors
          = RoomAnchors {
              _unRoomAnchors :: Data.Map.Strict.Map RoomAnchor ObjectAnchors
            } deriving stock Show

data Scene = Scene
  {_sceneTitle'         :: Text
  , _sceneDescription'  :: Text
  , _roomAnchored'      :: Maybe (NonEmpty (Text,[SceneAnchored])) -- text is Room area preamble
  , _floor'             :: Maybe (NonEmpty Text)
  , _visibleExits'      :: Maybe (NonEmpty Text)
  } deriving stock Show

data SceneAnchored = SceneAnchored {
  _sceneAnchored' :: Text
, _sceneRelated' :: [Text]
} deriving stock Show

data World = World
  { _objectMap'         :: GIDToDataMapping Object
  , _locationMap'       :: GIDToDataMapping Location
  , _descriptiveMap'    :: LabelToGIDListMapping Adjective Object
  , _exitMap'           :: GIDToDataMapping Exit
  }

instance Show ContainedIn where
  show (ContainedIn _ containedIn) = show containedIn

newtype Objects action
  = Objects {_unObjects' :: Data.List.NonEmpty.NonEmpty (GID Object)}
      deriving stock Show

data Verbosity
  = Quiet
  | Loud
  | Normal
    deriving stock Show

instance Eq Nexus where
  (Nexus _) == (Nexus _) = True

instance Ord Nexus where
  compare (Nexus _) (Nexus _) = EQ
  (<=) (Nexus _) (Nexus _) = True

directionFromRoomAnchor :: RoomAnchor -> Text
directionFromRoomAnchor roomAnchor =
  Data.Text.toLower . fst $ Data.Text.breakOn "Anchor" (toText $ show roomAnchor)

instance ToText (Label Object) where
  toText :: Label Object -> Text
  toText = toText . _unLabel'

instance ToText (Label Location) where
  toText :: Label Location -> Text
  toText = toText . _unLabel'

instance ToText RoomAnchor where
  toText :: RoomAnchor -> Text
  toText = toText . show

getLocationIdM :: GameStateExceptT (GID Location)
getLocationIdM =
  _playerLocation' . _player' <$> get

getLocationM :: GID Location -> GameStateExceptT Location
getLocationM gid = do
  world <- _world' <$> get
  throwMaybeM errmsg $ Data.Map.Strict.lookup gid (unLocationMap world)
  where
    unLocationMap = _unGIDToDataMapping' . _locationMap'
    errmsg = "that location wasn't found"

updateLocationM :: GID Location -> Location -> GameStateExceptT ()
updateLocationM gidLocation location = do
  world <- _world' <$> get
  let (GIDToDataMapping locationMap) = _locationMap' world
      updatedMap = GIDToDataMapping
                    $ Data.Map.Strict.insert gidLocation location locationMap
  modify' (\gs -> gs{_world' = world{_locationMap' = updatedMap}})

throwMaybeM :: Text -> Maybe a -> GameStateExceptT a
throwMaybeM _ (Just a) = pure a
throwMaybeM errmsg Nothing  = throwError errmsg

throwLeftM :: Text -> Either (a :: Type) (b :: Type) -> GameStateExceptT b
throwLeftM _ (Right b)     = pure b
throwLeftM errmsg (Left _) = throwError errmsg

throwRightM :: Text -> Either (a :: Type) (b :: Type) -> GameStateExceptT a
throwRightM _ (Left a)       = pure a
throwRightM errmsg (Right _) = throwError errmsg

setVerbosityM :: GameStateExceptT ()
setVerbosityM = do
  (verbosity,report) <- setVerbose . _verbosity' <$> get
  liftIO $ print report
  modify' (\gs -> gs{_verbosity' = verbosity})
  pass

setVerbose :: Verbosity -> (Verbosity,Text)
setVerbose Quiet = (Normal, "verbosity set to normal")
setVerbose Normal = (Loud, "verbosity set to loud")
setVerbose Loud = (Quiet, "verbosity set to quiet")

class VisibleObject a where
  isVisible :: a -> GameStateExceptT Bool