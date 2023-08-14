module HauntedHouse.Game.Engine.OnlyVerb.DoLook where

import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Recognizer (NounPhrase)
import HauntedHouse.Game.Narration (displaySceneM)
import HauntedHouse.Game.Location (getLocationM, getLocationIdM)

doLookM :: GameStateExceptT ()
doLookM = getLocationIdM >>= getLocationM >>= displaySceneM False 
 
doLookObjectM :: NounPhrase -> GameStateExceptT ()
doLookObjectM _ = pass
  
printTitleM :: Text -> GameStateExceptT ()
printTitleM title = print (("You're in the " :: String) <> show title) 

printDescriptionM :: Text -> GameStateExceptT () 
printDescriptionM _ = pass 