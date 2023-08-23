module HauntedHouse.Game.Engine.OnlyVerb.DoLook where

import HauntedHouse.Game.Model.World -- (GameStateExceptT)



doLookM :: GameStateExceptT ()
doLookM = pass -- getLocationIdM >>= getLocationM >>= displaySceneM False 
 