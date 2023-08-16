module HauntedHouse.Game.Engine.OnlyVerb.DoLook where

import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Recognizer (NounPhrase)
import HauntedHouse.Game.Narration (displaySceneM)
import HauntedHouse.Game.Location (getLocationM, getLocationIdM)

doLookM :: GameStateExceptT ()
doLookM = getLocationIdM >>= getLocationM >>= displaySceneM False 
 