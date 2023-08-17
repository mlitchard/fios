module HauntedHouse.Game.Narration.Anchored where

import HauntedHouse.Game.Model.World
        (RoomAnchors (..), ObjectAnchors (..), RoomAnchor (..)
        , Neighbors (..), Object (..), Proximity (..)
        , Location (_anchoredObjects'), fromProximity)
import HauntedHouse.Game.Model (GameStateExceptT, GameState)
import qualified Data.Map.Strict (toList)
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Mapping (NeighborMap(..))

import qualified Data.List.NonEmpty
import HauntedHouse.Game.Object (getObjectM)
import qualified Data.List.Split
import qualified Data.Text
import HauntedHouse.Game.Model.Display (Display)

directionFromRoomAnchor :: RoomAnchor -> Text
directionFromRoomAnchor roomAnchor = 
  Data.Text.toLower . fst $ Data.Text.breakOn "Anchor" (show roomAnchor)

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

