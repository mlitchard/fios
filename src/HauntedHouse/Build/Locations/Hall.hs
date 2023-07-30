module HauntedHouse.Build.Locations.Hall where

import HauntedHouse.Build.LocationTemplate
import HauntedHouse.Build.Locations.BuildFrame 
import HauntedHouse.Game.Location
import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Build.LocationLabels (hallLabel)

-- _locationMap'       :: GIDToDataMapping Location
buildHall :: GameStateExceptT ()
buildHall= do
  location <- getLocation hallGID 
  buildFrame hallGID hallLabel location