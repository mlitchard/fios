module HauntedHouse.Game.Narration.Anchored where 
  
import HauntedHouse.Game.Model.World
        (RoomAnchors (..), ObjectAnchors (..), RoomAnchor (..)
        , Neighbors (..), Object (..), Proximity (..))
import HauntedHouse.Game.Model (GameStateExceptT)
import qualified Data.Map.Strict (toList)
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Mapping (NeighborMap(..))
import HauntedHouse.Game.Narration.Containers (displayContainer, displayPortal)
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Object (getObjectM, displayObjectM)

displayAnchoredM :: RoomAnchors -> GameStateExceptT ()
displayAnchoredM (RoomAnchors roomAnchorMap) = do
  mapM_ displayByLocation $ Data.Map.Strict.toList roomAnchorMap

displayByLocation :: (RoomAnchor, ObjectAnchors) -> GameStateExceptT ()
displayByLocation (key, ObjectAnchors objectRelationsMap) = do
  liftIO $ print (("In the " :: String) <> show key <> " you see")
  mapM_ displayByLocationArea $ Data.Map.Strict.toList objectRelationsMap

{-

data Object = Object
  { _shortName'     :: Text
  , _moveability'   :: Moveability
  , _containment'   :: Maybe (Either Container Portal)
  , _odescription'  :: Text
  , _descriptors    :: [Label Adjective]
  } deriving stock Show

-}

displayByLocationArea :: (GID Object, Neighbors) -> GameStateExceptT ()
displayByLocationArea (objectGID, Neighbors (NeighborMap relations)) = do
  (Object shortName _ mContainment description _) <- getObjectM objectGID
  print (shortName <> "\n")
  print (description <> "\n")
  whenJust mContainment (either displayContainer displayPortal)
  mapM_ (displayRelations shortName) $ Data.Map.Strict.toList relations

displayRelations :: Text
                      -> (Proximity, Data.List.NonEmpty.NonEmpty (GID Object))
                      -> GameStateExceptT ()
displayRelations shortName (proximity,gidObjects)= do
  print (show proximity <> " " <> shortName)
  mapM_ displayObjectM gidObjects
