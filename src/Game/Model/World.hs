module Game.Model.World where

import Game.Model.Mapping
import Recognizer (Adjective, Imperative, NounPhrase, Verb, PrepPhrase, AdjPhrase)
import Game.Model.Condition (Proximity, Moveability)
import System.Console.Haskeline (InputT)
import Text.Show (Show(..))
import Prelude hiding (show)
import qualified Data.List.NonEmpty
import Game.Model.GID (GID)
import qualified Data.Map.Strict
import qualified Data.Text
import Control.Monad.Except (MonadError(throwError))
import Tokenizer (Lexeme (..))

newtype ObjectAnchorMap = 
  ObjectAnchorMap (GIDToDataMap Object (Maybe (NonEmpty (GID Object))))

type GameStateExceptT = ReaderT Config (ExceptT Text (StateT GameState IO))

type InputGameStateExceptT = InputT GameStateExceptT

newtype AnchoredTo = AnchoredTo
  { _unAnchoredTo' :: Data.Map.Strict.Map (GID Object) (GID Object,Proximity)}
    deriving stock (Show, Eq, Ord)

type ClarifyWhich = (Imperative -> GameStateExceptT ())
                      -> (Label Object, NonEmpty (GID Object, Object))
                      -> GameStateExceptT ()


data Config = Config {
  _primaryEvaluator'         :: Imperative -> GameStateExceptT ()
  , _clarifyWhich'           :: ClarifyWhich
  , _evalVerbNounPhrase'     :: (Verb, NounPhrase) -> GameStateExceptT ()
  , _evalVerbPrepPhrase'     :: (Verb, PrepPhrase) -> GameStateExceptT ()
  , _evalVerbTwoPrepPhrases' :: EvalVerbThree
  , _evalVerbPhraseFive'     :: EvalVerbFive
  , _evalVerbPhraseSeven'    :: EvalVerbSeven
}

newtype Container = Container 
  { _unContainer' :: Map (Label Object) (NonEmpty ContainedEntity)} 

data ContainedPlacement = On | In deriving stock Show
 
data ContainedEntity = ContainedEntity {
    _containedGid' :: GID Object
  , _placement' :: ContainedPlacement 
} 

type EvalVerbThree = (Verb, PrepPhrase, PrepPhrase) -> GameStateExceptT ()

newtype Exit = Exit { _toDestination' :: GID Location} deriving stock Show

type EvalVerbSeven = (Verb, NounPhrase, PrepPhrase) -> GameStateExceptT ()

type EvalVerbFive = (Verb, AdjPhrase) -> GameStateExceptT ()

newtype ExitGIDMap
  = ExitGIDMap {_unExitGIDMap' :: LabelToGIDMapping Exit Object}
      deriving stock Show

data FoundAnchoredTo = FoundAnchoredTo
  {  _anchoredObject' :: (GID Object,Object)
  ,  _proximityTo' :: (GID Object, Proximity)
  }

instance Show FoundAnchoredTo where
  show (FoundAnchoredTo (gid, _) (gid',prox)) =
    show gid <> " " <> show gid' <> " " <> show prox

data GameState = GameState
  { _world'                 :: World
  , _report'                :: [Text]
  , _player'                :: Player
  , _narration'             :: Narration
  , _verbosity'             :: Verbosity
  , _evaluator'             :: Imperative -> GameStateExceptT ()
  , _clarification'         :: Maybe Clarification
  , _clarifiedDirectObject' :: Maybe (GID Object, Object)
  , _displayAction'         :: GameStateExceptT ()
  }

type GIDObjectPair = (GID Object,Object)
data Clarification = Clarification {
    _clarifyingLabel' :: Label Object
  , _gidObjectPairs' :: NonEmpty (GID Object,Object)
}
report :: GameStateExceptT ()
report = do
  report' <- _report' <$> get
  mapM_ print report'

data Location = Location {
    _title'           :: Text
  , _description'     :: Text
  , _anchoredObjects' :: RoomSectionMap
  , _objectLabelMap'  :: LabelToGIDListMapping Object Object
  , _directions'      :: Maybe ExitGIDMap
}

data Lockability = Locked | UnLocked | NotLockable deriving stock Show

data Narration = Narration {
      _playerAction' :: Data.List.NonEmpty.NonEmpty Text
    , _enviroment'   :: Data.List.NonEmpty.NonEmpty Text
    , _npcResponse' :: Data.List.NonEmpty.NonEmpty Text
    , _scene'       :: Scene
  } deriving stock Show

data Object = Object {
    _shortName'       :: Text
  , _entityLabel'     :: Label Object
  , _odescription'    :: [Text]
  , _descriptives'    :: [Label Adjective]
  , _moveability'     :: Moveability
  , _orientation'     :: Orientation
  , _standardActions'  :: StandardActions
}

data StandardActions = StandardActions
  {   _getAction' :: GetAction 
    , _putAction' :: PutAction
    , _lookAction' :: LookAction
    , _openAction' :: OpenAction
    , _closeAction' :: CloseAction
    , _lockAction' :: LockAction
    , _unlockAction' :: UnlockAction
    , _goAction' :: GoAction
  }

data GetAction = GetAction 
  { _updateGet' :: GameStateExceptT ()
  -- object taken from    -- object taken
  , _get' :: Object -> GameStateExceptT ()
  }

data PutPrep
  = PutOn 
  | PutIn 
  | PutTo  

toPutPrep :: Lexeme -> Maybe PutPrep 
toPutPrep ON = Just PutOn 
toPutPrep IN = Just PutIn 
toPutPrep TO = Just PutTo 
toPutPrep _  = Nothing 

data PutAction = PutAction 
  {   _updatePut' :: GameStateExceptT ()
    , _put' :: GID Object -> PutPrep -> GameStateExceptT ()
  }

type LookF = (Object -> Map (GID Object) Container -> GameStateExceptT ())
                                                                                
newtype LookAtF = LookAtF {_unLookAt' :: LookF} 
newtype LookOnF = LookOnF {_unLookOn' :: LookF}
newtype LookInF = LookInF {_unLookIn' :: LookF}

data LookAction = LookAction { 
      _updatePerception'  :: UpdatePerceptionFunctions
    , _perception'        :: PerceptionFunctions 
    , _lookFunctions'     :: LookFunctions  
  } 

data LookFunctions = LookFunctions {
    _lookAt'          :: LookAtF
  , _lookIn'          :: LookInF 
  , _lookOn'          :: LookOnF
}

type DisplayF = Text -> Maybe Text

data PerceptionFunctions = PerceptionFunctions {
    _lookPerceptionF'     :: LookF -> LookF
  , _displayPerceptionF'  :: DisplayF
}

data UpdatePerceptionFunctions = UpdatePerceptionFunctions {
    _updateBlockReport' :: Object -> GameStateExceptT () 
  , _updateDisplay' :: Object -> GameStateExceptT () 
}

data OpenAction = OpenAction 
  { _updateOpen' :: GameStateExceptT ()
  , _open' :: Object -> GameStateExceptT ()
  }

data CloseAction = CloseAction 
  { _updateClose' :: GameStateExceptT ()
  , _close' :: Object -> GameStateExceptT ()
  }

data LockAction = LockAction 
  { _updateLock' :: GameStateExceptT ()
  , _lock' :: GameStateExceptT ()
  }
data UnlockAction = UnlockAction 
  { _updateUnlock' :: GameStateExceptT ()
  , _unlock' :: GameStateExceptT ()
  }

data GoPrep
  = GoUp
  | GoDown 
  | GoThrough 
  | GoIn 

data GoAction = GoAction 
  { _updateGo' :: GameStateExceptT ()
  , _go' :: GID Object -> GoPrep -> GameStateExceptT ()
  }

type AnchorMap = Data.Map.Strict.Map (GID Object) (Maybe (NonEmpty Anchored))

newtype ObjectAnchors = ObjectAnchors { _unObjectAnchors' :: AnchorMap } 

data Anchored = Anchored 
  { _gid' :: GID Object
  , _proximity :: Proximity
  }

data OpenState = Open | Closed deriving stock Show

data Orientation
  = ContainedBy' (GameStateExceptT (GID Object, ContainedPlacement))
  | Inventory
  | Anchor (GameStateExceptT (Maybe (NonEmpty Anchored)))
  | AnchoredTo' (GameStateExceptT Proximity)

data Player = Player
  { _playerLocation'  :: GID Location
  , _p_inv'           :: [GID Object]
  } deriving stock Show

data RoomSection
  = NorthSection
  | SouthSection
  | WestSection
  | EastSection
  | NorthWestSection
  | NorthEastSection
  | SouthWestSection
  | SouthEastSection
    deriving stock (Show,Eq,Ord)

type RoomSectionMap = Data.Map.Strict.Map RoomSection ObjectAnchors

data Scene = Scene
  {_sceneTitle'         :: Text
  , _sceneDescription'  :: Text
  , _roomAnchored'      :: [DescribeRoomSection] -- text is Room area preamble
  , _visibleExits'      :: Maybe (NonEmpty Text)
  } deriving stock Show

data DescribeRoomSection = DescribeRoomSection
  {   _preamble' :: Text
    , describeAnchoredObjects :: [DescribeAnchor]
  } deriving stock Show

data DescribeAnchor = DescribeAnchor 
  { _anchorDesc'    :: Text
  , _maybeShelf'    :: Maybe (Text, NonEmpty Text) 
  , _anchoredDesc'  :: Maybe (NonEmpty DescribeAnchored)
  } deriving stock Show
  
data DescribeAnchored = DescribeAnchored
  {_prelude' :: (Text, Text)
  , _maybeShelf' :: Maybe (Text, NonEmpty Text)
  } deriving stock Show

data World = World
  { _objectMap'         :: GIDToDataMap Object Object
  , _containerMap'      :: GIDToDataMap Object Container
  , _locationMap'       :: GIDToDataMap Location Location
  , _descriptiveMap'    :: LabelToGIDListMapping Adjective Object
  , _exitMap'           :: GIDToGIDMapping Object Location
  }

newtype Objects action
  = Objects {_unObjects' :: Data.List.NonEmpty.NonEmpty (GID Object)}
      deriving stock Show

data Verbosity
  = Quiet
  | Loud
  | Normal
    deriving stock Show

directionFromRoomSection :: RoomSection -> Text
directionFromRoomSection roomAnchor =
  Data.Text.toLower . fst $ Data.Text.breakOn "Section" (toText $ show roomAnchor)

instance ToText (Label Object) where
  toText :: Label Object -> Text
  toText = toText . _unLabel'

instance ToText (Label Location) where
  toText :: Label Location -> Text
  toText = toText . _unLabel'

instance ToText RoomSection where
  toText :: RoomSection -> Text
  toText = toText . show

getLocationIdM :: GameStateExceptT (GID Location)
getLocationIdM =
  _playerLocation' . _player' <$> get

getLocationM :: GID Location -> GameStateExceptT Location
getLocationM gid = do
  world <- _world' <$> get
  throwMaybeM errmsg $ Data.Map.Strict.lookup gid (unLocationMap world)
  where
    unLocationMap = _unGIDToDataMap' . _locationMap'
    errmsg = "that location wasn't found"

updateLocationM :: GID Location -> Location -> GameStateExceptT ()
updateLocationM gidLocation location = do
  world <- _world' <$> get
  let (GIDToDataMap locationMap) = _locationMap' world
      updatedMap = GIDToDataMap
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
  (verbosity,report') <- setVerbose . _verbosity' <$> get
  liftIO $ print report'
  modify' (\gs -> gs{_verbosity' = verbosity})
  pass

setVerbose :: Verbosity -> (Verbosity,Text)
setVerbose Quiet = (Normal, "verbosity set to normal")
setVerbose Normal = (Loud, "verbosity set to loud")
setVerbose Loud = (Quiet, "verbosity set to quiet")

class VisibleObject a where
  isVisible :: a -> GameStateExceptT Bool