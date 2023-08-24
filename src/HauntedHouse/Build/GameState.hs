module HauntedHouse.Build.GameState where

import HauntedHouse.Build.Locations.Kitchen ( buildKitchen )
import HauntedHouse.Game.Model.World (GameStateExceptT)
-- import HauntedHouse.Build.Locations.Hall (buildHall)

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