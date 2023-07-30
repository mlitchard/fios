module HauntedHouse.Game.Build.GameState where

import HauntedHouse.Game.Build.Locations.Kitchen ( buildKitchen )
import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Game.Build.Locations.Hall (buildHall)

buildGameState :: GameStateExceptT ()
buildGameState = do 
  liftIO $ print ("buildGameState" :: String)
  buildWorld
  gs <- get 
  print gs
  finalizeBuild

{-

data GameState = GameState
  { _world'         :: World
  , _report'        :: [Text]
  , _player'        :: Player
  , _narration'     :: Narration
  , _newScene'      :: Bool
  , _clarification' :: Maybe (NonEmpty Text)
  }

-}

finalizeBuild :: GameStateExceptT ()
finalizeBuild = pass
    

buildWorld :: GameStateExceptT () 
buildWorld = do 
  buildKitchen
  buildHall
