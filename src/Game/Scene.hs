module Game.Scene where
import Game.Model.World
        (GameStateExceptT, RoomAnchors (..), RoomAnchor (..), ObjectAnchors (..)
        , directionFromRoomAnchor, Neighbors (..), Object (..)
        , SceneAnchored (..), Container (..), Shelf (..), Orientation (..))
import qualified Data.Map.Strict
import Game.Model.GID (GID)
import Game.Model.Mapping (NeighborMap(..), GIDList, ContainerMap (..))
import Game.Object (getObjectM, getShortNameM)
import Game.Model.Condition (Proximity (..), Perceptibility (..))
import qualified Data.List.NonEmpty
import Data.These (These(..))
import qualified Data.Text
import Game.World (findShelfM)

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


-- newtype Container = Container
--  { _unContainer' :: These ContainedIn Shelf } deriving stock Show

isRoomAnchor :: Object -> Bool 
isRoomAnchor (Object {..}) = case _orientation' of 
  (Anchored _) -> True 
  _             -> False

displayProximity :: Proximity -> Text
displayProximity PlacedIn = "In it"
displayProximity PlacedOn = "On it"
displayProximity PlacedUnder = "Under it"
displayProximity PlacedAbove = "Above it"
displayProximity PlacedLeft = "To the left of it"
displayProximity PlacedRight = "To the right of it"
displayProximity PlacedFront = "In front of it"
displayProximity PlacedBehind = "Behind it"
