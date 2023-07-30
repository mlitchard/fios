module HauntedHouse.Build.GameState where

import HauntedHouse.Build.Locations.Kitchen ( buildKitchen )
import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Build.Locations.Hall (buildHall)

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
