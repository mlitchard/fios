module HauntedHouse.Game.Narration where

import HauntedHouse.Game.Model
        (Narration (..), GameStateExceptT, GameState (..))
import HauntedHouse.Game.Model.World (Location (..), RoomAnchors (..), Objects (..))
import HauntedHouse.Game.Narration.Anchored (displayAnchoredM)
import qualified Data.Map.Strict (null)

updateNarration :: Narration -> GameStateExceptT ()
updateNarration narration =  modify' (\g -> g {_narration' = narration})

displaySceneM :: Location -> GameStateExceptT ()
displaySceneM (Location title description anchored floor' _ _ _) = do
  liftIO $ print ("You are in the " <> title)
  liftIO $ print description
  liftIO $ print ("This is what you see" :: Text)
  if emptyRoom
    then print ("An empty room" :: Text)
    else displayAnchoredM anchored
  where
    (RoomAnchors anchored') = anchored
    emptyRoom = Data.Map.Strict.null anchored' && isNothing floor'