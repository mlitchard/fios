module HauntedHouse.Game.Narration where

import HauntedHouse.Game.Model
        (Narration (..), GameStateExceptT, GameState (..))
import HauntedHouse.Game.Model.World (Location (..), RoomAnchors (..))
import HauntedHouse.Game.Narration.Anchored (displayAnchoredM)
import qualified Data.Map.Strict (null)

updateNarration :: Narration -> GameStateExceptT ()
updateNarration narration =  modify' (\g -> g {_narration' = narration})

displaySceneM :: Location -> GameStateExceptT ()
displaySceneM (Location title description anchored floor' _) = do
  liftIO $ print ("You are in the " <> title <> "\n")
  liftIO $ print (description <> "\n")
  liftIO $ print ("This is what you see\n" :: Text)
  if emptyRoom
    then print ("An empty room" :: String)
    else displayAnchoredM anchored
  --liftIO $ print $ maybe emptyRoom objectDescriptions objects
  where
    (RoomAnchors anchored') = anchored
    emptyRoom = Data.Map.Strict.null anchored' && isNothing floor'