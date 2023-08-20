module HauntedHouse.Game.Model.World where

import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Recognizer (Adjective)
import HauntedHouse.Game.Model.Interface (Exit, Interface, Portal)
import HauntedHouse.Game.Model.Condition (Condition, Inventory, Proximity)
import System.Console.Haskeline (InputT)
import Text.Show (Show(..))
import Prelude hiding (show)
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Model.GID (GID)
import Data.These (These)
import qualified Data.Map.Strict
import qualified Data.Text
import Control.Monad.Error (throwError)

type GameStateExceptT = 
        ExceptT Text (StateT GameState IO)

type InputGameStateExceptT a = InputT GameStateExceptT a 

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

newtype ObjectAnchors
          = ObjectAnchors {
              _unObjectAnchors :: Data.Map.Strict.Map 
                                    (GID Object) 
                                    (Neighbors Object)
            } deriving stock Show

newtype Neighbors object = Neighbors 
  {_unNeighbors' :: NeighborMap Proximity object} deriving stock Show 

data GameState = GameState
  { _world'         :: World 
  , _report'        :: [Text]
  , _player'        :: Player
  , _narration'     :: Narration
  , _verbosity'     :: Verbosity
  , _clarification' :: Maybe (NonEmpty Text)
  }

data Verbosity
  = Quiet
  | Loud
  | Normal
    deriving stock Show

data Scene = Scene
  {_sceneTitle'         :: Text
  , _sceneDescription'  :: Text
  , _sceneAnchored'     :: Data.List.NonEmpty.NonEmpty Text
  , _sceneRelated'      :: Data.List.NonEmpty.NonEmpty Text
  , _visibleExits'      :: Data.List.NonEmpty.NonEmpty Text
  } deriving stock Show

data Narration = Narration
  {_playerAction' :: Data.List.NonEmpty.NonEmpty Text
  ,_enviroment'   :: Data.List.NonEmpty.NonEmpty Text
  , _npcResponse' :: Data.List.NonEmpty.NonEmpty Text
  , _scene'       :: Scene
  } deriving stock Show
  
data Player = Player
  { _playerLocation'  :: GID Location
  , _p_inv'           :: Maybe (Data.List.NonEmpty.NonEmpty (GID Object))
  } deriving stock Show


data World = World
  { _objectMap'         :: GIDToDataMapping Object
  , _locationMap'       :: GIDToDataMapping Location
  , _descriptiveMap'    :: LabelToGIDListMapping Adjective Object
  , _exitMap'           :: GIDToDataMapping (Exit Location)
  }

data Location = Location {
  _title' :: Text 
  , _description'     :: Text
  , _anchoredObjects' :: RoomAnchors
}

data MetaCondition = MetaCondition {
   _condition :: Condition Object 
  , _setCondition :: GameStateExceptT ()
  }
 {-
 data Location objectAnchors object = Location
  { _title'           :: Text
  , _description'     :: Text
  , _anchoredObjects' :: RoomAnchors objectAnchors
  , _anchoredTo'      :: AnchoredTo object
  , _floorInventory'  :: Maybe (Data.List.NonEmpty (GID object))
  , _objectLabelMap'  :: LabelToGIDListMapping object object
  , _directions'      :: Maybe (ExitGIDMap exit object)
  } deriving stock Show
-}
data Object = Object {
    _shortName'       :: Text
  , _odescription'    :: Text
  , _descriptives'    :: [Label Adjective]
  , _metaConditions'  :: [MetaCondition]
  , _nexus'           :: Maybe Nexus  
}

newtype Containment = Containment 
  { _unContainment' :: These ContainedIn
                              (Either ContainedOn ContainedBy)
  } deriving stock Show

data ContainedIn = ContainedIn 
  { _containerInterface'  :: Interface (GameStateExceptT ())-- toClose toOpen' unlock lock
  , _containedIn'         :: ContainerMap Object
  }

instance Show ContainedIn where 
  show (ContainedIn _ containedIn) = show containedIn 

data ContainedBy = ContainedBy 
  { _containedBy' :: Either Inventory (GID Object)
  , _objectContained' :: GID Object
  } deriving stock (Show)

newtype ContainedOn = ContainedOn {_unContainedOn' :: ContainerMap Object}
  deriving stock (Eq,Ord,Show)

newtype Objects action
  = Objects {_unObjects' :: Data.List.NonEmpty.NonEmpty (GID Object)}
      deriving stock Show

newtype Nexus = Nexus {
    _unNexus' :: Either Containment (Portal Location)
  }

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
getLocationIdM = do
  _playerLocation' . _player' <$> get

--  _locationMap'       :: GIDToDataMapping Location
getLocationM :: GID Location -> GameStateExceptT Location
getLocationM gid = do
  world <- _world' <$> get
  throwMaybeM errmsg $ Data.Map.Strict.lookup gid (unLocationMap world)
  where
    unLocationMap = _unGIDToDataMapping' . _locationMap'
    errmsg = "that location wasn;t found"

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