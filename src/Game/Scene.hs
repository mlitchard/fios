module Game.Scene where
import Game.Model.World
import qualified Data.Map.Strict
import Game.Model.GID (GID (..))
import Game.Model.Mapping
import Game.Object (getObjectM)
import Game.Model.Condition (Proximity (..))
import qualified Data.List.NonEmpty
-- text builder for Scene

display :: Object -> Text
display Object {..} = "a " <> _shortName'

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
                  <> " you see"

describeAnchor :: (GID Object, Maybe (NonEmpty Anchored))
                    -> GameStateExceptT DescribeAnchor
describeAnchor (anchorGid,mAnchoredGids) = do
  (Object {..}) <- getObjectM anchorGid
  descAnch <- case mAnchoredGids of
                Nothing -> pure Nothing
                Just anchGids -> describeAnchoreds _shortName' anchGids
  shelfObjects <- tryDescribeShelf anchorGid
  pure $ DescribeAnchor ("a " <> _shortName') shelfObjects descAnch

describeAnchoreds :: Text
                      -> NonEmpty Anchored
                      -> GameStateExceptT (Maybe (NonEmpty DescribeAnchored))
describeAnchoreds shortName anchoredGids = do
  removeNonPerceptible <$> mapM (describeAnchored shortName) anchoredGids
  where
    removeNonPerceptible = (nonEmpty <$> catMaybes) . Data.List.NonEmpty.toList
-- ToDo fix visibililty. Ought not to assume visible
describeAnchored :: Text
                      -> Anchored
                      -> GameStateExceptT (Maybe DescribeAnchored)
describeAnchored shortName (Anchored gid proximity) = do
  anchored <- getObjectM gid
  let display' = tryDisplayF anchored
  case display' of
    Nothing -> pure Nothing
    Just anchored' -> do
                        let proximityPair = (proximityDesc, "is a " <> _shortName' anchored')
                        shelfContents <- tryDescribeShelf gid
                        let shelfDescription = shelfPrelude <$> shelfContents
                        (pure . Just) (DescribeAnchored proximityPair shelfDescription)
  where
    shelfPrelude (shelfName, contents)
      = (,) ("On the " <> shelfName <> " you see:") contents
    proximityDesc = displayProximity proximity <> shortName

shallowDescribeObject :: Object -> GameStateExceptT (Maybe Text)
shallowDescribeObject entity =
  pure (display <$> tryDisplayF entity)

tryDescribeShelf :: GID Object -> GameStateExceptT (Maybe (Text,NonEmpty Text))
tryDescribeShelf gid = do
  (GIDToDataMap worldCMap) <- _containerMap' . _world' <$> get
  let maybeCmap = Data.Map.Strict.lookup gid worldCMap
  case maybeCmap of
    Just ((Container cmap)) -> do
                                shortName <- _shortName' <$> getObjectM gid
                                case filtShelf of
                                  Nothing -> pure Nothing
                                  Just filtered -> do
                                                    content <- mapM describeShelfContents filtered
                                                    pure $ Just (shortName,content)
                                where
                                  concatElems = concatMap toList
                                                  $ Data.Map.Strict.elems cmap
                                  filtShelf = nonEmpty $ filter isShelf concatElems
    Nothing -> pure Nothing
    where
      isShelf (ContainedEntity _ On) = True
      isShelf _ = False

describeShelfContents :: ContainedEntity -> GameStateExceptT Text
describeShelfContents (ContainedEntity gid _) = do
  shortName <$> getObjectM gid
  where
    shortName (Object {..} )= "a " <> _shortName'


tryDisplayF :: Object -> Maybe Object
tryDisplayF entity@(Object {..}) =
  displayF entity
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
