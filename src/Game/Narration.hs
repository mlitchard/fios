module Game.Narration where

import Game.Model.World
import qualified Data.Map.Strict (toList)
import Game.Scene ( makeRoomSectionDescriptions )
import Game.Model.Mapping
import Game.Object (namedDirectionM, getShortNameM)
import Game.Model.GID
import qualified Data.List.NonEmpty
import Game.Model.Display (updateEnvironmentM)

updateNarration :: Narration -> GameStateExceptT ()
updateNarration narration =  modify' (\g -> g {_narration' = narration})

reportFloorM :: [GID Object] -> GameStateExceptT [Text]
reportFloorM = mapM getShortNameM

makeSceneM :: Location -> GameStateExceptT ()
makeSceneM (Location title desc anchObj _ d) = do
  narration <- _narration' <$> get
  roomAnchors <- makeRoomSectionDescriptions anchObj
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
  updateEnvironmentM ("You are in the " <> title)

displayNormalM :: Scene -> GameStateExceptT ()
displayNormalM (Scene title desc _   exit) =
  updateEnvironmentM ("You are in the " <> title)
  >> updateEnvironmentM desc >> displayExitsM exit

displayLoudM :: Scene -> GameStateExceptT ()
displayLoudM (Scene title desc anchored exit) = do
  updateEnvironmentM ("You are in the " <> title)
  updateEnvironmentM desc
  updateEnvironmentM ("This is what you see" :: Text)
  let showItemsM = case anchored of
                    [] -> updateEnvironmentM ("An Empty Room" :: Text)
                    a  -> mapM_ displayDescribeRoomSectionM a
  showItemsM >> displayExitsM exit

displayExitsM :: Maybe (NonEmpty Text) -> GameStateExceptT ()
displayExitsM Nothing = updateEnvironmentM ("There's no visible exits." :: Text)
displayExitsM (Just exits) = mapM_ updateEnvironmentM exits

{-
data DescribeRoomSection = DescribeRoomSection
  {   _preamble' :: Text
    , describeAnchoredObjects :: [DescribeAnchor]
-}

displayDescribeRoomSectionM :: DescribeRoomSection -> GameStateExceptT ()
displayDescribeRoomSectionM (DescribeRoomSection preamble anchoredObjects) =
 updateEnvironmentM preamble
    >>  mapM_ displayDescribeAnchorM anchoredObjects

{-
data DescribeAnchor = DescribeAnchor 
  { _anchorDesc'    :: Text
  , _maybeShelf'    :: Maybe [Text]  
  , _anchoredDesc'  :: [DescribeAnchored]
  } deriving stock Show
-}
-- DescribeAnchor
displayDescribeAnchorM :: DescribeAnchor -> GameStateExceptT ()
displayDescribeAnchorM (DescribeAnchor {..}) = do
  updateEnvironmentM _anchorDesc' >> whenJust _maybeShelf' displayShelf
  whenJust _anchoredDesc' (mapM_ displayDescribeAnchored)
  pass

displayShelf :: (Text, NonEmpty Text) -> GameStateExceptT ()
displayShelf (preamble,inv) = 
  updateEnvironmentM preamble 
    >> mapM_ updateEnvironmentM inv
{-
data DescribeAnchored = DescribeAnchored
  {_prelude' :: (Text, Text)
  , _maybeShelf' :: Maybe (Text, [Text])
  } deriving stock Show
-}
displayDescribeAnchored :: DescribeAnchored -> GameStateExceptT ()
displayDescribeAnchored (DescribeAnchored {..}) = do
  updateEnvironmentM (fst _prelude')
    >> updateEnvironmentM (snd _prelude')
    >> maybeShelf
  where
    maybeShelf = case _maybeShelf' of
                  Just (shelf,inv) -> updateEnvironmentM (shelfPrelude shelf)
                                        >> mapM_ updateEnvironmentM inv
                  Nothing -> pass
    shelfPrelude shelf = "On the " <> shelf <> "you see:"
-- displayFloorM :: [Text] -> GameStateExceptT ()
-- displayFloorM = mapM_ updateEnvironemtM 