module HauntedHouse.Game.Narration where

import HauntedHouse.Game.Model
        (Narration (..), GameStateExceptT, GameState (..), Verbosity (..))
import HauntedHouse.Game.Model.World (Location (..), RoomAnchors (..), Objects (..))
import HauntedHouse.Game.Narration.Anchored (displayAnchoredM)
import qualified Data.Map.Strict (null)
import HauntedHouse.Tokenizer.Data (Lexeme(VERBOSE))

updateNarration :: Narration -> GameStateExceptT ()
updateNarration narration =  modify' (\g -> g {_narration' = narration})

displaySceneM :: Location -> GameStateExceptT ()
displaySceneM (Location title description anchored floor' _ _ _) = do
  verbosity <- _verbosity' <$> get 
  case verbosity of 
    Quiet -> displayQuietM
    Normal -> displayNormalM
    Loud -> displayLoudM 
  where
    (RoomAnchors anchored') = anchored
    emptyRoom = Data.Map.Strict.null anchored' && isNothing floor'
    displayQuietM = liftIO $ print ("You are in the " <> title)
    displayNormalM = 
      liftIO $ print ("You are in the " <> title)
      >> print description
    displayLoudM = do
      liftIO $ print ("You are in the " <> title)
      liftIO $ print description
      liftIO $ print ("This is what you see" :: Text)
      testEmpty 

    testEmpty =
      if emptyRoom
        then print ("An empty room" :: Text)
        else displayAnchoredM anchored