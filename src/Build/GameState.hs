module Build.GameState where

import Build.Locations.Kitchen ( buildKitchen )
import Game.Model.World (GameStateExceptT)
-- import Build.Locations.Hall (buildHall)

buildGameState :: GameStateExceptT ()
buildGameState = do 
  buildWorld 
  finalizeBuild
  
finalizeBuild :: GameStateExceptT ()
finalizeBuild = pass
    
buildWorld :: GameStateExceptT () 
buildWorld = do  
  buildKitchen
--  buildHall