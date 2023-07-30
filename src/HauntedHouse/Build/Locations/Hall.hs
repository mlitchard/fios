module HauntedHouse.Build.Locations.Hall where

import HauntedHouse.Build.LocationTemplate
import HauntedHouse.Build.Locations.BuildFrame 
import HauntedHouse.Game.Location
import HauntedHouse.Game.Model (GameStateExceptT)

-- _locationMap'       :: GIDToDataMapping Location
buildHall :: GameStateExceptT ()
buildHall= do
  location <- getLocation hallGID 
  buildFrame hallGID location