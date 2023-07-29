module HauntedHouse.Game.Build.Locations.Hall where

import qualified Data.Map.Strict
import qualified Data.List.NonEmpty

import HauntedHouse.Game.Build.DirectionTemplate
import HauntedHouse.Game.Build.LocationTemplate
import HauntedHouse.Game.Build.Locations.BuildFrame 
import HauntedHouse.Game.Location
import HauntedHouse.Game.Model (GameStateExceptT, GameState (..))
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.World

import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.World


-- _locationMap'       :: GIDToDataMapping Location
buildHall :: GameStateExceptT ()
buildHall= do
  location <- getLocation hallGID 
  buildFrame hallGID location
  -- buildHallFrame

