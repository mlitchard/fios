module HauntedHouse.Game.Engine.OnlyVerb.DoLook where

import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Recognizer (NounPhrase)

doLookM :: GameStateExceptT ()
doLookM = do
  print ("entered doLook" :: String)
  pass

doLookObjectM :: NounPhrase -> GameStateExceptT ()
doLookObjectM _ = pass
  
printTitleM :: Text -> GameStateExceptT ()
printTitleM title = print (("You're in the " :: String) <> show title) 

printDescriptionM :: Text -> GameStateExceptT () 
printDescriptionM _ = pass 