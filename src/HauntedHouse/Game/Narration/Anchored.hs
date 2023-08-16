module HauntedHouse.Game.Narration.Anchored where

import HauntedHouse.Game.Model.World
        (RoomAnchors (..), ObjectAnchors (..), RoomAnchor (..)
        , Neighbors (..), Object (..), Proximity (..), Location (_anchoredObjects'), fromProximity)
import HauntedHouse.Game.Model (GameStateExceptT)
import qualified Data.Map.Strict (toList)
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Mapping (NeighborMap(..))
import HauntedHouse.Game.Narration.Containers (displayContainment, displayPortal)
import qualified Data.List.NonEmpty
import HauntedHouse.Game.Object (getObjectM, displayObjectM)
import qualified Data.List.Split
import qualified Data.Text

displayAnchoredM :: RoomAnchors -> GameStateExceptT ()
displayAnchoredM (RoomAnchors roomAnchorMap) = do
  mapM_ displayByLocation $ Data.Map.Strict.toList roomAnchorMap

displayByLocation :: (RoomAnchor, ObjectAnchors) -> GameStateExceptT ()
displayByLocation (key, ObjectAnchors objectRelationsMap) = do
  liftIO $ print (("In the " :: Text) <> displayRoomAnchor key <> " you see")
  mapM_ displayByLocationArea $ Data.Map.Strict.toList objectRelationsMap

displayRoomAnchor :: RoomAnchor -> Text
displayRoomAnchor roomAnchor = 
  Data.Text.toLower . fst $ Data.Text.breakOn "Anchor" (show roomAnchor)

displayByLocationArea :: (GID Object, Neighbors) -> GameStateExceptT ()
displayByLocationArea (objectGID, Neighbors (NeighborMap relations)) = do
  (Object shortName _ mContainment description _) <- getObjectM objectGID
  print shortName
  print description
  whenJust mContainment (either displayContainment displayPortal)
  mapM_ (displayRelations shortName) $ Data.Map.Strict.toList relations

displayRelations :: Text
                      -> (Proximity, Data.List.NonEmpty.NonEmpty (GID Object))
                      -> GameStateExceptT ()
displayRelations shortName (proximity,gidObjects)= do
  print (fromProximity proximity <> " " <> shortName)
  mapM_ displayObjectM gidObjects
