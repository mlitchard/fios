module HauntedHouse.Game.Engine.OnlyVerb.DoLook where

import HauntedHouse.Game.Model.World ( Location(..), Objects (..)) 
import HauntedHouse.Game.Location (getLocationId, getLocation)
import HauntedHouse.Game.Model (GameStateExceptT)

doLook :: GameStateExceptT ()
doLook = do
  print ("entered doLook" :: String)
  gid <- getLocationId
  location' <- getLocation gid
  printTitle (_title' location') 
  printDescription (_description' location')
  printObjects (_objects' location')
  pass
  
printTitle :: Text -> GameStateExceptT ()
printTitle _ = pass 

printDescription :: Text -> GameStateExceptT () 
printDescription _ = pass 

printObjects :: Maybe Objects -> GameStateExceptT () 
printObjects _ = pass


