module Game.Narration where

import Game.Model.World
import qualified Data.Map.Strict (toList)
import Game.Scene (ScenePart(makeScenePart))
import Game.Model.Mapping
import Game.Object (namedDirectionM, getShortNameM)
import Game.Model.GID
import qualified Data.List.NonEmpty

updateNarration :: Narration -> GameStateExceptT ()
updateNarration narration =  modify' (\g -> g {_narration' = narration})

reportFloorM :: [GID Object] -> GameStateExceptT [Text]
reportFloorM = mapM getShortNameM

makeSceneM :: Location -> GameStateExceptT ()
makeSceneM (Location title desc anchObj anchTo olmap d) = do
  narration <- _narration' <$> get
  roomAnchors <- makeScenePart anchObj
  exits <- makeExits d
  modify (\gs -> gs {_narration' =
                        narration {_scene' =
                          scene roomAnchors exits}})
  where
    scene roomAnchors exits = Scene {
      _sceneTitle' = title
      , _sceneDescription' = desc
      , _roomAnchored' =  roomAnchors
      , _visibleExits' = exits
    }

makeExits :: Maybe ExitGIDMap -> GameStateExceptT (Maybe (NonEmpty Text))
makeExits Nothing = pure Nothing
makeExits (Just (ExitGIDMap ( LabelToGIDMapping exitGIDMap))) = do
  let exitGIDMap' :: [(Label Exit, GID Object)]
      exitGIDMap' = Data.Map.Strict.toList exitGIDMap
  raw <- mapM namedDirectionM exitGIDMap'
  pure $ Just $ Data.List.NonEmpty.fromList $ map exitText raw

exitText :: (Text, Label Exit) -> Text
exitText (shortName, Label exit) =
  "to the " <> toText exit <> "is a " <> shortName

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
displayQuietM (Scene title _ _ _ ) =
  liftIO $ print ("You are in the " <> title)

displayNormalM :: Scene -> GameStateExceptT ()
displayNormalM (Scene title desc _   exit) =
  print ("You are in the " <> title) >> print desc >> displayExitsM exit

displayLoudM :: Scene -> GameStateExceptT ()
displayLoudM (Scene title desc anchored exit) = do
  print ("You are in the " <> title)
  print desc
  print ("This is what you see" :: Text)
  let showItems = case anchored of
                    Nothing -> print ("An Empty Room" :: Text)
                    Just a  -> mapM_ displayAnchoredM a
  showItems >> displayExitsM exit

displayExitsM :: Maybe (NonEmpty Text) -> GameStateExceptT ()
displayExitsM Nothing = print ("There's no visible exits." :: Text)
displayExitsM (Just exits) = mapM_ print exits

displayAnchoredM :: (Text, [SceneAnchored]) -> GameStateExceptT ()
displayAnchoredM (preamble,xs) =
  print preamble >> mapM_ displaySceneAnchoredM xs

displaySceneAnchoredM :: SceneAnchored -> GameStateExceptT ()
displaySceneAnchoredM (SceneAnchored anchor related inventory) =
  print anchor
    >> mapM_ print inventory 
    >> mapM_ print related

displayFloorM :: [Text] -> GameStateExceptT ()
displayFloorM = mapM_ print