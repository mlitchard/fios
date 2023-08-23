module HauntedHouse.Game.Scene where
import HauntedHouse.Game.Model.World (GameStateExceptT, RoomAnchors (..), RoomAnchor, ObjectAnchors (..), directionFromRoomAnchor, Neighbors (..), Object (..), SceneAnchored)
import qualified Data.Map.Strict
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Mapping (NeighborMap(..))
import HauntedHouse.Game.Object (getObjectM)

-- text builder for Scene

{-

instance Display RoomAnchors where

  displayScene :: RoomAnchors -> GameStateExceptT ()
  displayScene (RoomAnchors roomAnchorMap) = do
    mapM_ displayScene $ Data.Map.Strict.toList roomAnchorMap
  
  display :: RoomAnchors -> GameStateExceptT ()
  display = displayScene

instance Display (RoomAnchor, ObjectAnchors) where

  displayScene :: (RoomAnchor, ObjectAnchors) -> GameStateExceptT ()
  displayScene (key, ObjectAnchors objectRelationsMap) = do
    liftIO 
      $ print (("In the " :: Text) <> directionFromRoomAnchor key <> " you see")
    mapM_ displayScene $ Data.Map.Strict.toList objectRelationsMap
  
  display = displayScene
-}
{-

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
class Scene a where
  type RenderAs a
  makeScene :: a -> GameStateExceptT (RenderAs a)

instance Scene RoomAnchors where
  type RenderAs RoomAnchors = [(Text,[SceneAnchored])]
  makeScene :: RoomAnchors -> GameStateExceptT [(Text,[SceneAnchored])]
  makeScene (RoomAnchors roomAnchorMap) = pure mempty 
   -- mapM makeScene (Data.Map.Strict.toList roomAnchorMap)
{-
instance Scene (RoomAnchor, ObjectAnchors) where
  makeScene :: (RoomAnchor, ObjectAnchors) -> GameStateExceptT Text
  makeScene (roomAnchor,objectAnchors) = pure scene
    where
      preamble = "In the " <> directionFromRoomAnchor  roomAnchor <> "you see"
      scene = preamble


newtype ObjectAnchors = ObjectAnchors { 
  _unObjectAnchors :: Data.Map.Strict.Map (GID Object) Neighbors 
  } deriving stock Show

-}
{-
instance Scene ObjectAnchors where 
  makeScene :: ObjectAnchors -> GameStateExceptT Text
  makeScene (ObjectAnchors objectAnchors) = 
    unlines <$> mapM makeScene (Data.Map.Strict.toList objectAnchors)
-}
{-

  displayScene :: (GID Object, Neighbors) -> GameStateExceptT ()
  displayScene (objectGID, Neighbors (NeighborMap relations)) = do
    (Object shortName _ mContainment description _) <- getObjectM objectGID
    print shortName
    print description
    whenJust mContainment (either display display)
    mapM_ display
      $ toDisplayRelation shortName
      <$> Data.Map.Strict.toList relations
    where
      toDisplayRelation shortName (proximity, proximitObjects) = DisplayRelation
        {_proximity' = proximity
        , _proximitObjects' = proximitObjects
        , _proximitShortName' = shortName}


data Object = Object {
    _shortName'       :: Text
  , _odescription'    :: [Text]
  , _descriptives'    :: [Label Adjective]
  , _containment'     :: Containment
  , _metaConditions'  :: [MetaCondition]
}
-}{-
instance Scene (GID Object, Neighbors) where 
  makeScene :: (GID Object, Neighbors) -> GameStateExceptT Text 
  makeScene (objectGID,Neighbors (NeighborMap relations)) = do 
    
    objectScene <- makeScene objectGID
    if (null objectScene) 
      then pure mempty 
      else do 
-}
    

