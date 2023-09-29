module Game.Scene where
import Game.Model.World
import qualified Data.Map.Strict
import Game.Model.GID (GID (..))
import Game.Model.Mapping (GIDList, GIDToDataMap (..), ContainerMap (..))
import Game.Object (getObjectM, getObjectGIDPairM)
import Game.Model.Condition (Proximity (..))
import qualified Data.List.NonEmpty
import Control.Monad.Except (MonadError(..))
-- text builder for Scene

display :: Text -> Text
display shortName = "a " <> shortName

makeScene :: RoomAnchors -> GameStateExceptT [DescribeRoomAnchor]
makeScene  (RoomAnchors roomAnchorMap)
  | Data.Map.Strict.null roomAnchorMap = pure mempty
  | otherwise = mapM describeRoomAnchor (Data.Map.Strict.toList roomAnchorMap)

data DescribeRoomAnchor = DescribeRoomAnchor
  {   _preamble' :: Text
    , describeAnchoredObjects :: [DescribeAnchor]
  }
describeRoomAnchor :: (RoomAnchor, ObjectAnchors)
                    -> GameStateExceptT DescribeRoomAnchor
describeRoomAnchor (roomAnchor,objectAnchors) = do
  describedEntityAnchors <- describeAnchors objectAnchors
  pure $ DescribeRoomAnchor preamble describedEntityAnchors 
  where
    preamble = "In the "
                  <> directionFromRoomAnchor  roomAnchor
                  <> "you see"

data DescribeAnchor = DescribeAnchor 
  { _anchorDesc'    :: Text
  , _maybeShelf'    :: Maybe [Text]  
  , _anchoredDesc'  :: [DescribeAnchored]
  }
describeAnchors :: ObjectAnchors
                    -> GameStateExceptT [DescribeAnchor]
describeAnchors (ObjectAnchors (_,objectAnchors)) = do
  mapM describeAnchor (Data.List.NonEmpty.toList objectAnchors)

describeAnchor gid = do
  entity@(Object {..}) <- getObjectM gid
  describedAnchoreds <- describeAnchoreds gid _orientation' _shortName'
  shelfObjects :: Maybe [Text] <- tryDescribeShelf gid
  pure $ DescribeAnchor ("a " <> _shortName') shelfObjects describedAnchoreds
  
describeAnchoreds :: GID Object 
                      -> Orientation 
                      -> Text 
                      -> GameStateExceptT [DescribeAnchored]
describeAnchoreds gid (Anchor (lid, _)) shortName = do
  (Location {..}) <- getLocationM lid
  locationObjectGids <- fmap Data.List.NonEmpty.toList
                          <$> mapM getObjectGIDPairM
                          $ _objectLabelMap'._unLabelToGIDListMapping'
  let anchored = mapMaybe (\(gid',e) -> (,) gid' <$> findAnchored gid e) locationObjectGids
  res <- mapM (describeAnchored shortName) anchored
  pure mempty


describeAnchoreds _ _ shortName =
  throwError ("describeAnchored being used on a non-anchor" <> shortName)

data DescribeAnchored = DescribeAnchored
  {_prelude' :: (Text, Text)
  , _maybeShelf' :: Maybe (Text, [Text])
  }
describeAnchored :: Text
                      -> (GID Object, (Object, Proximity))
                      -> GameStateExceptT DescribeAnchored
describeAnchored shortName (gid, (Object {..}, proximity)) = do
  shelfContents <- tryDescribeShelf gid
  let shelfDescription = (,) shelfPrelude <$> shelfContents
  pure (DescribeAnchored proximityPair shelfDescription)
  where
    shelfPrelude = "On the " <> _shortName' <> " you see:"
    proximityDesc = displayProximity proximity <> shortName
    proximityPair = (proximityDesc, "is a " <> _shortName')

shallowDescribeObject :: Object -> GameStateExceptT (Maybe Text)
shallowDescribeObject (Object {..}) = pure $ displayF $ display _shortName'
  where
    displayF = _standardActions'._lookAction'._perception'._displayPerceptionF'

findAnchored :: GID Object -> Object  -> Maybe (Object, Proximity)
findAnchored gid entity@(Object _ _ _ _ _ (AnchoredTo' (gid', proximity)) _)
  | gid == gid' = Just (entity,proximity)
findAnchored _ _ = Nothing

tryDescribeShelf :: GID Object -> GameStateExceptT (Maybe [Text])
tryDescribeShelf gid = do
  (GIDToDataMap worldCMap) <- _containerMap' . _world' <$> get
  let maybeCmap = Data.Map.Strict.lookup gid worldCMap
  case maybeCmap of
    Just ((Container (ContainerMap cmap))) -> Just . concat
                                                <$> mapM tryDescribeShelfContents
                                                (Data.Map.Strict.elems cmap)
    Nothing -> pure Nothing

tryDescribeShelfContents :: GIDList Object
                              -> GameStateExceptT [Text]
tryDescribeShelfContents xs = do
  entities <- mapM getObjectM xs
  pure $ map ("a " <> _shortName')
        $ Data.List.NonEmpty.filter
            (\(Object {..}) -> isShelfInventory _orientation') entities


isShelfInventory :: Orientation -> Bool
isShelfInventory (ContainedBy' (ContainedBy (On _) _)) = True
isShelfInventory _ = False

tryDisplay :: Object -> Maybe Text
tryDisplay (Object {..}) =
  displayF (display _shortName')
  where
    displayF = _standardActions'._lookAction'._perception'._displayPerceptionF'


{-
class ScenePart a where
  type RenderAs a
  makeScenePart :: a -> GameStateExceptT (RenderAs a)
{-
instance ScenePart RoomAnchors where
  type RenderAs RoomAnchors = Maybe (NonEmpty (Text, [SceneAnchored]))
  makeScenePart :: RoomAnchors
                    -> GameStateExceptT (Maybe (NonEmpty (Text, [SceneAnchored])))
  makeScenePart (RoomAnchors roomAnchorMap) =
    if Data.Map.Strict.null roomAnchorMap
      then pure Nothing
      else Just . Data.List.NonEmpty.fromList
            <$> mapM makeScenePart (Data.Map.Strict.toList roomAnchorMap)
   -- mapM makeScene (Data.Map.Strict.toList roomAnchorMap)
-}
{-
instance ScenePart (RoomAnchor, ObjectAnchors) where

  type RenderAs (RoomAnchor, ObjectAnchors) = (Text,RenderAs ObjectAnchors)

  makeScenePart (roomAnchor,objectAnchors) = do
    sceneAnchored <- makeScenePart objectAnchors
    pure (preamble,sceneAnchored)
    where
      preamble = case roomAnchor of
                  CenterAnchor -> mempty
                  _ -> "In the "
                          <> directionFromRoomAnchor  roomAnchor
                          <> "you see"
-}
{-
instance ScenePart ObjectAnchors where
  type RenderAs ObjectAnchors = [SceneAnchored]

  makeScenePart (ObjectAnchors objectAnchors) = do
    catMaybes <$> mapM makeScenePart (Data.Map.Strict.toList objectAnchors)
-}


instance ScenePart (GID Object, Neighbors) where

  type RenderAs (GID Object, Neighbors) = Maybe SceneAnchored
  makeScenePart (gid, neighbors) = error "undefined" {- do
    object <- getObjectM gid
    case _perceptability' object of
      Perceptible -> do
                        neighborPart <- makeScenePart neighbors
                        inventory <- findShelfM gid
                        pure . Just
                          $ SceneAnchored (_shortName' object) inventory neighborPart
      Imperceptible -> pure Nothing
-}
instance ScenePart Neighbors where

  type RenderAs Neighbors = [Text]
  makeScenePart (Neighbors (NeighborMap nmap)) =
    catMaybes <$> mapM makeScenePart (Data.Map.Strict.toList nmap)

instance ScenePart (Proximity, GIDList Object) where

  type RenderAs (Proximity, GIDList Object) = Maybe Text

  makeScenePart (proximity, gidList) = do
    let x :: [GID Object]
        x = Data.List.NonEmpty.toList gidList
    entities <- filter (\(_,e) -> (not . isRoomAnchor) e) <$> mapM getObjectGIDPairM x
    res <- catMaybes <$> mapM makeScenePart entities
    pure $ if null res
      then Nothing
      else Just (displayProximity proximity <> unlines res)

-- Entity can't be a RoomAnchor
instance ScenePart (GID Object, Object) where

  type RenderAs (GID Object, Object) = Maybe Text
  makeScenePart (gid,Object {..}) = error ("undefined")
    {-
    case _perceptability' of
      Perceptible   -> do
                          mShelf <- presentShelfContents gid _shortName'
                          pure (mShelf <|> Just _shortName')
      Imperceptible -> pure Nothing
-}
presentShelfContents :: GID Object -> Text -> GameStateExceptT (Maybe Text)
presentShelfContents gid shortName = do
  mEntities <- findShelfM gid
  case mEntities of
    (Just entities) -> do
                          let prelude = "On the " <> shortName <> "you see:"
                          pure (Just (prelude <> entities))
    Nothing -> pure Nothing
-}

displayProximity :: Proximity -> Text
displayProximity PlacedIn = "In the "
displayProximity PlacedOn = "On the "
displayProximity PlacedUnder = "Under the "
displayProximity PlacedAbove = "Above the "
displayProximity PlacedLeft = "To the left of the "
displayProximity PlacedRight = "To the right of the "
displayProximity PlacedFront = "In front of the "
displayProximity PlacedBehind = "Behind the "
