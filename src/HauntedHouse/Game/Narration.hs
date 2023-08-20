module HauntedHouse.Game.Narration where

import HauntedHouse.Game.World
        (Narration (..), GameStateExceptT, GameState (..), Verbosity (..))
import HauntedHouse.Game.Model.World (Location (..), RoomAnchors (..), Objects (..))
import HauntedHouse.Game.Narration.Anchored
import qualified Data.Map.Strict (null)
import HauntedHouse.Tokenizer.Data (Lexeme(VERBOSE))
import HauntedHouse.Game.Model.Display (Display(..))

updateNarration :: Narration -> GameStateExceptT ()
updateNarration narration =  modify' (\g -> g {_narration' = narration})

displaySceneM :: Bool -> Location -> GameStateExceptT ()
displaySceneM useVerbosity location =
  if useVerbosity
    then do
            verbosity <- _verbosity' <$> get 
            case verbosity of 
              Quiet -> displayQuietM
              Normal -> displayNormalM
              Loud -> displayLoudM
  else displayLoudM  
  where
    title = _title' location
    description = _description' location
    anchored = _anchoredObjects' location
    floor' = _floorInventory' location
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
        else display anchored -- add displayFloorM