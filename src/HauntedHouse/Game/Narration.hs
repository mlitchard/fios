module HauntedHouse.Game.Narration where

import HauntedHouse.Game.World

import HauntedHouse.Game.Model.World -- (Location (..), RoomAnchors (..), Objects (..), GameStateExceptT)
import qualified Data.Map.Strict (null)
import HauntedHouse.Tokenizer.Data (Lexeme(VERBOSE))
import HauntedHouse.Game.Scene (ScenePart(makeScenePart))
import qualified Data.List

updateNarration :: Narration -> GameStateExceptT ()
updateNarration narration =  modify' (\g -> g {_narration' = narration})

makeSceneM :: Location -> GameStateExceptT ()
makeSceneM (Location title desc anchObj anchTo floor' olmap d) = do
  narration <- _narration' <$> get
  roomAnchors <- makeScenePart anchObj
  modify (\gs -> gs {_narration' = narration {_scene' = scene roomAnchors}})
  where
    scene roomAnchors = Scene {
      _sceneTitle' = title
      , _sceneDescription' = desc
      , _roomAnchored' =  roomAnchors
      , _floor' = mempty
      , _visibleExits' = mempty
    }

displaySceneM :: Bool -> GameStateExceptT ()
displaySceneM useVerbosity  =  do
  narration <- _narration' <$> get
  verbosity <- _verbosity' <$> get
  let scene = _scene' narration
      display = if useVerbosity
                  then do
                        case verbosity of
                          Quiet -> displayQuietM
                          Normal -> displayNormalM
                          Loud -> displayLoudM
                  else displayLoudM
  display scene

displayQuietM :: Scene -> GameStateExceptT ()
displayQuietM (Scene title _ _ _ _) =
  liftIO $ print ("You are in the " <> title)

displayNormalM :: Scene -> GameStateExceptT ()
displayNormalM (Scene title desc _ _  exit) =
  print ("You are in the " <> title) >> print desc >> displayExitsM exit 

displayLoudM :: Scene -> GameStateExceptT ()
displayLoudM (Scene title desc anchored floor' exit) = do
  print ("You are in the " <> title)
  print desc
  print ("This is what you see" :: Text)
  let showItems = case (anchored,floor') of 
                    (Nothing,Nothing) -> print ("An Empty Room" :: Text)
                    (Just a, Just fl) -> mapM_ displayAnchoredM a 
                                          >> displayFloorM fl
                    (Just a,Nothing)  -> mapM_ displayAnchoredM a 
                    (Nothing, Just fl) -> displayFloorM fl 
  showItems >> displayExitsM exit
                    
displayExitsM :: Maybe (NonEmpty Text) -> GameStateExceptT ()
displayExitsM Nothing = print ("There's no visible exits." :: Text)
displayExitsM (Just exits) = mapM_ print exits 

displayAnchoredM :: (Text, [SceneAnchored]) -> GameStateExceptT ()
displayAnchoredM (preamble,xs) = 
  print preamble >> mapM_ displaySceneAnchoredM xs

{-

data SceneAnchored = SceneAnchored {
  _sceneAnchored' :: Text
, _sceneRelated' :: [Text] 
} deriving stock Show

-}
displaySceneAnchoredM :: SceneAnchored -> GameStateExceptT () 
displaySceneAnchoredM (SceneAnchored anchor related) = 
  print anchor >> mapM_ print related 

displayFloorM :: NonEmpty Text -> GameStateExceptT () 
displayFloorM = mapM_ print 