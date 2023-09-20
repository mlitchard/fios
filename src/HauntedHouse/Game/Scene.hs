module HauntedHouse.Game.Scene where
import HauntedHouse.Game.Model.World
        (GameStateExceptT, RoomAnchors (..), RoomAnchor (..), ObjectAnchors (..)
        , directionFromRoomAnchor, Neighbors (..), Object (..)
        , SceneAnchored (..), Nexus (..), Containment (..), Shelf (..), Orientation (..))
import qualified Data.Map.Strict
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Mapping (NeighborMap(..), GIDList, ContainerMap (..))
import HauntedHouse.Game.Object (getObjectM, getShortNameM)
import HauntedHouse.Game.Model.Condition (Proximity (..), Perceptibility (..))
import qualified Data.List.NonEmpty
import Data.These (These(..))
import qualified Data.Text

-- text builder for Scene

class ScenePart a where
  type RenderAs a
  makeScenePart :: a -> GameStateExceptT (RenderAs a)

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


instance ScenePart ObjectAnchors where
  type RenderAs ObjectAnchors = [SceneAnchored]

  makeScenePart (ObjectAnchors objectAnchors) = do
    catMaybes <$> mapM makeScenePart (Data.Map.Strict.toList objectAnchors)

instance ScenePart (GID Object, Neighbors) where

  type RenderAs (GID Object, Neighbors) = Maybe SceneAnchored
  makeScenePart (gid, neighbors) = do
    object <- getObjectM gid
    case _perceptability' object of
      Perceptible -> do
                        neighborPart <- makeScenePart neighbors
                        inventory <- findShelfM gid 
                        pure . Just
                          $ SceneAnchored (_shortName' object) inventory neighborPart
      Imperceptible -> pure Nothing

instance ScenePart Neighbors where

  type RenderAs Neighbors = [Text]
  makeScenePart (Neighbors (NeighborMap nmap)) =
    catMaybes <$> mapM makeScenePart (Data.Map.Strict.toList nmap)

instance ScenePart (Proximity, GIDList Object) where

  type RenderAs (Proximity, GIDList Object) = Maybe Text

  makeScenePart (proximity, gidList) = do
    let x :: [GID Object]
        x = Data.List.NonEmpty.toList gidList
    objects <- filter (not . isRoomAnchor) <$> mapM getObjectM x 
    res <- catMaybes <$> mapM makeScenePart objects
    pure $ if null res
      then Nothing
      else Just (displayProximity proximity <> unlines res)

-- Entity can't be a RoomAnchor
instance ScenePart Object where

  type RenderAs Object = Maybe Text
  makeScenePart entity = do
    case _perceptability' entity of
      Perceptible   -> do
                        let prelude = _shortName' entity <> "\n"
                        pure (Just prelude)
      Imperceptible -> pure Nothing


-- newtype Containment = Containment
--  { _unContainment' :: These ContainedIn Shelf } deriving stock Show

isRoomAnchor :: Object -> Bool 
isRoomAnchor (Object {..}) = case _orientation' of 
  (Anchoring _) -> True 
  _             -> False

findShelfM :: GID Object -> GameStateExceptT (Maybe Text)
findShelfM gid = do
  (Object {..}) <- getObjectM gid
  case _mNexus' of
    Nothing -> pure Nothing -- NoOp not a container 
    Just (Containment' containment) -> do
                                         contents <-
                                          findShelfObjectsM containment
                                         pure
                                          $ makeDesc _shortName' <$> contents
    Just _ -> pure Nothing -- NoOp not a Shelf 

    where
    makeDesc :: Text -> [Text] -> Text
    makeDesc shortname contents =
      let prelude = "On the " <> shortname <> " you see: "
      in unlines (prelude : contents)

findShelfObjectsM :: Containment -> GameStateExceptT (Maybe [Text])
findShelfObjectsM (Containment containment) = case containment of
  This _ -> pure Nothing
  That shelf -> makeDescShelfM shelf
  These _ shelf -> makeDescShelfM shelf

makeDescShelfM ::Shelf -> GameStateExceptT (Maybe [Text])
makeDescShelfM (Shelf _ (ContainerMap cmap)) = do
  descriptions <- mapM getShortNameM elems'
  if null descriptions
    then pure Nothing
    else pure (Just descriptions)
  where
    elems' = concatMap Data.List.NonEmpty.toList $ Data.Map.Strict.elems cmap

displayProximity :: Proximity -> Text
displayProximity PlacedOn = "On it"
displayProximity PlacedUnder = "Under it"
displayProximity PlacedAbove = "Above it"
displayProximity PlacedLeft = "To the left of it"
displayProximity PlacedRight = "To the right of it"
displayProximity PlacedFront = "In front of it"
displayProximity PlacedBehind = "Behind it"
