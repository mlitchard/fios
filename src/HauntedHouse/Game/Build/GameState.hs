module HauntedHouse.Game.Build.GameState where

import HauntedHouse.Game.Build.Locations.Kitchen ( buildKitchen )
import HauntedHouse.Game.Model (GameStateExceptT)

buildGameState :: GameStateExceptT ()
buildGameState = do 
  buildWorld
  finalizeBuild

finalizeBuild :: GameStateExceptT ()
finalizeBuild = pass

buildWorld :: GameStateExceptT () 
buildWorld = do 
  buildKitchen
  