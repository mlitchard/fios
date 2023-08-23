module HauntedHouse.Game.Scene where
import HauntedHouse.Game.Model.World
        (GameStateExceptT, RoomAnchors (..), RoomAnchor, ObjectAnchors (..)
        , directionFromRoomAnchor, Neighbors (..), Object (..)
        , SceneAnchored (..), isPerceived)
import qualified Data.Map.Strict
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Mapping (NeighborMap(..), GIDList)
import HauntedHouse.Game.Object (getObjectM)
import HauntedHouse.Game.Model.Condition (Proximity (..))
import qualified Data.List.NonEmpty

-- text builder for Scene

class ScenePart a where
  type RenderAs a
  makeScenePart :: a -> GameStateExceptT (RenderAs a)

instance ScenePart RoomAnchors where
  type RenderAs RoomAnchors = [(Text,[SceneAnchored])]
  makeScenePart :: RoomAnchors -> GameStateExceptT [(Text,[SceneAnchored])]
  makeScenePart (RoomAnchors roomAnchorMap) =
    mapM makeScenePart (Data.Map.Strict.toList roomAnchorMap)
   -- mapM makeScene (Data.Map.Strict.toList roomAnchorMap)

instance ScenePart (RoomAnchor, ObjectAnchors) where

  type RenderAs (RoomAnchor, ObjectAnchors) = (Text,RenderAs ObjectAnchors)

  makeScenePart (roomAnchor,objectAnchors) = do
    sceneAnchored <- makeScenePart objectAnchors
    pure (preamble,sceneAnchored)
    where
      preamble = "In the " <> directionFromRoomAnchor  roomAnchor <> "you see"

{-

newtype ObjectAnchors = ObjectAnchors { 
  _unObjectAnchors :: Data.Map.Strict.Map (GID Object) Neighbors 
  } deriving stock Show

-}
instance ScenePart ObjectAnchors where
  type RenderAs ObjectAnchors = [SceneAnchored]

  makeScenePart (ObjectAnchors objectAnchors) =
    catMaybes <$> mapM makeScenePart (Data.Map.Strict.toList objectAnchors)

instance ScenePart (GID Object, Neighbors) where

  type RenderAs (GID Object, Neighbors) = Maybe SceneAnchored
  makeScenePart (gid, neighbors) = do
    object <- getObjectM gid
    if any isPerceived (_metaConditions' object)
      then do
              neighborPart <- makeScenePart neighbors
              pure $ Just $ SceneAnchored mempty neighborPart
      else pure Nothing

instance ScenePart Neighbors where

  type RenderAs Neighbors = [Text]
  makeScenePart (Neighbors (NeighborMap nmap)) =
    catMaybes <$> mapM makeScenePart (Data.Map.Strict.toList nmap)


instance ScenePart (Proximity, GIDList Object) where

  type RenderAs (Proximity, GIDList Object) = Maybe Text

  makeScenePart (proximity, gidList) = do
    let x :: [GID Object]
        x = Data.List.NonEmpty.toList gidList

    res <- catMaybes <$> mapM makeScenePart x
    pure $ if null res 
      then Nothing
      else Just (displayProximity proximity <> unlines res)

instance ScenePart (GID Object) where

  type RenderAs (GID Object) = Maybe Text
  makeScenePart gid = do
    object <- getObjectM gid
    pure $ if any isPerceived (_metaConditions' object)
      then Just $ (_shortName' object <> "\n")
      else Nothing

displayProximity :: Proximity -> Text
displayProximity PlacedOn = "On it"
displayProximity PlacedUnder = "Under it"
displayProximity PlacedAbove = "Above it"
displayProximity PlacedLeft = "To the left of it"
displayProximity PlacedRight = "To the right of it"
displayProximity PlacedFront = "In front of it"
displayProximity PlacedBack = "Behind it"
