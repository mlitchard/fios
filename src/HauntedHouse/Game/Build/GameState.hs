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

finalizeBuild :: GameStateExceptT ()
finalizeBuild = pass
    
buildWorld :: GameStateExceptT () 
buildWorld = do 
  buildKitchen
  buildHall
