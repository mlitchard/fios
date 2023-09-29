module Build.Locations.Hall where
{-
import Build.LocationTemplate
import Build.Locations.BuildFrame 
import Game.Location
import Game.Model (GameStateExceptT)
import Build.LocationLabels (hallLabel)
-}
{-
-- _locationMap'       :: GIDToDataMap Location
buildHall :: GameStateExceptT ()
buildHall= do
  location <- getLocation hallGID 
  buildFrame hallGID hallLabel location
-}