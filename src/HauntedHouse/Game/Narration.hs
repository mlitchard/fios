module HauntedHouse.Game.Narration where

import HauntedHouse.Game.World

import HauntedHouse.Game.Model.World -- (Location (..), RoomAnchors (..), Objects (..), GameStateExceptT)
import qualified Data.Map.Strict (null)
import HauntedHouse.Tokenizer.Data (Lexeme(VERBOSE))

updateNarration :: Narration -> GameStateExceptT ()
updateNarration narration =  modify' (\g -> g {_narration' = narration})

{-
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

  data Location = Location {
    _title'             :: Text
  , _description'     :: Text
  , _anchoredObjects' :: RoomAnchors
  , _anchoredTo'      :: AnchoredTo Object
  , _floorInventory'  :: Maybe (Data.List.NonEmpty.NonEmpty (GID Object))
  , _objectLabelMap'  :: LabelToGIDListMapping Object Object
  , _directions'      :: Maybe (ExitGIDMap Exit Object)
  } 

data SceneAnchored = SceneAnchored {
  _sceneAnchored' :: Text
, _sceneRelated' :: [Text] 
} deriving stock Show

data Scene = Scene
  {_sceneTitle'         :: Text
  , _sceneDescription'  :: Text
  , _roomAnchored'      :: [(Text,[SceneAnchored])] -- text is Room area preamble
  , _floor'             :: [Text]
  , _visibleExits'      :: [Text]
  } deriving stock Show
-}
makeSceneM :: Location -> GameStateExceptT ()
makeSceneM location@(Location title desc anchObj anchTo floor' olmap d) = do
  narration <- _narration' <$> get 
  modify (\gs -> gs {_narration' = narration {_scene' = scene}})
  where
    scene = Scene {
      _sceneTitle' = title 
      , _sceneDescription' = desc 
      , _roomAnchored' = mempty 
      , _floor' = mempty 
      , _visibleExits' = mempty 
    }
{-
displaySceneM :: Bool -> Location -> GameStateExceptT ()
displaySceneM useVerbosity location =
  if useVerbosity
    then do
            verbosity <- _verbosity' <$> get 
            case verbosity of 
              Quiet -> displayQuietM
              Normal -> displayNormalM
              Loud -> displayLoudM
  else displayLoudM  
  where
    title = _title' location
    description = _description' location
    anchored = _anchoredObjects' location
    floor' = _floorInventory' location
    (RoomAnchors anchored') = anchored
    emptyRoom = Data.Map.Strict.null anchored' && isNothing floor'
    displayQuietM = liftIO $ print ("You are in the " <> title)
    displayNormalM = 
      liftIO $ print ("You are in the " <> title)
      >> print description
    displayLoudM = do
      liftIO $ print ("You are in the " <> title)
      liftIO $ print description
      liftIO $ print ("This is what you see" :: Text)
      testEmpty 

    testEmpty =
      if emptyRoom
        then print ("An empty room" :: Text)
        else display anchored -- add displayFloorM

-}