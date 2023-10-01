module Game.Scene where
import Game.Model.World
import qualified Data.Map.Strict
import Game.Model.GID (GID (..))
import Game.Model.Mapping
import Game.Object (getObjectM)
import Game.Model.Condition (Proximity (..))
-- text builder for Scene

display :: Text -> Text
display shortName = "a " <> shortName

-- This module assumes all objects are perceivable 

makeRoomSectionDescriptions :: RoomSectionMap
                                -> GameStateExceptT [DescribeRoomSection]
makeRoomSectionDescriptions roomAnchorMap
  | Data.Map.Strict.null roomAnchorMap = pure mempty
  | otherwise = mapM describeRoomSection (Data.Map.Strict.toList roomAnchorMap)

describeRoomSection :: (RoomSection, ObjectAnchors)
                    -> GameStateExceptT DescribeRoomSection
describeRoomSection (roomAnchor,ObjectAnchors objectAnchors) = do
  describedEntityAnchors <- mapM describeAnchor $ Data.Map.Strict.toList objectAnchors
  pure $ DescribeRoomSection preamble describedEntityAnchors
  where
    preamble = "In the "
                  <> directionFromRoomSection  roomAnchor
                  <> "you see"

describeAnchor :: (GID Object, Maybe (NonEmpty Anchored))
                    -> GameStateExceptT DescribeAnchor
describeAnchor (anchorGid,mAnchoredGids) = do
  (Object {..}) <- getObjectM anchorGid
  descAnch <- case mAnchoredGids of
                Nothing -> pure Nothing
                Just anchGids -> Just
                                  <$> describeAnchoreds _shortName' anchGids
  shelfObjects <- tryDescribeShelf anchorGid
  pure $ DescribeAnchor ("a " <> _shortName') shelfObjects descAnch

describeAnchoreds :: Text
                      -> NonEmpty Anchored
                      -> GameStateExceptT (NonEmpty DescribeAnchored)
describeAnchoreds shortName anchoredGids = do
  mapM (describeAnchored shortName) anchoredGids

describeAnchored :: Text
                      -> Anchored
                      -> GameStateExceptT DescribeAnchored
describeAnchored shortName (Anchored gid proximity) = do
  (Object {..}) <- getObjectM gid
  let proximityPair = (proximityDesc, "is a " <> _shortName')
  shelfContents <- tryDescribeShelf gid
  let shelfDescription = shelfPrelude <$> shelfContents
  pure (DescribeAnchored proximityPair shelfDescription)
  where
    shelfPrelude (shelfName, contents)
      = (,) ("On the " <> shelfName <> " you see:") contents
    proximityDesc = displayProximity proximity <> shortName

shallowDescribeObject :: Object -> GameStateExceptT (Maybe Text)
shallowDescribeObject (Object {..}) = pure $ displayF $ display _shortName'
  where
    displayF = _standardActions'._lookAction'._perception'._displayPerceptionF'

tryDescribeShelf :: GID Object -> GameStateExceptT (Maybe (Text,[Text]))
tryDescribeShelf gid = do
  (GIDToDataMap worldCMap) <- _containerMap' . _world' <$> get
  let maybeCmap = Data.Map.Strict.lookup gid worldCMap
  case maybeCmap of
    Just ((Container cmap)) -> do
                                shortName <- _shortName' <$> getObjectM gid
                                content <- mapM describeShelfContents filtShelf
                                pure $ Just (shortName,content)
                                where
                                  concatElems = concatMap toList 
                                                  $ Data.Map.Strict.elems cmap
                                  filtShelf = filter isShelf concatElems
    Nothing -> pure Nothing
    where
      isShelf (ContainedEntity _ On) = True
      isShelf _ = False

describeShelfContents :: ContainedEntity -> GameStateExceptT Text
describeShelfContents (ContainedEntity gid _) = do
  shortName <$> getObjectM gid
  where
    shortName (Object {..} )= "a " <> _shortName'

tryDisplay :: Object -> Maybe Text
tryDisplay (Object {..}) =
  displayF (display _shortName')
  where
    displayF = _standardActions'._lookAction'._perception'._displayPerceptionF'

displayProximity :: Proximity -> Text
displayProximity PlacedIn = "In the "
displayProximity PlacedOn = "On the "
displayProximity PlacedUnder = "Under the "
displayProximity PlacedAbove = "Above the "
displayProximity PlacedLeft = "To the left of the "
displayProximity PlacedRight = "To the right of the "
displayProximity PlacedFront = "In front of the "
displayProximity PlacedBehind = "Behind the "
