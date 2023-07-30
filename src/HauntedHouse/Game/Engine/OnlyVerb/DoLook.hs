module HauntedHouse.Game.Engine.OnlyVerb.DoLook where

import HauntedHouse.Game.Model.World ( Location(_description') ) 
import HauntedHouse.Game.Location (getLocationId, getLocation)
import HauntedHouse.Game.Model (GameStateExceptT)

doLook :: GameStateExceptT ()
doLook = do
  print ("entered doLook" :: String)
  gid <- getLocationId
  location <- getLocation gid
  print (_description' location :: Text)


