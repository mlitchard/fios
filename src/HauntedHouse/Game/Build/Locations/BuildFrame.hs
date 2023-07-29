module HauntedHouse.Game.Build.Locations.BuildFrame 
  (buildFrame) where

import qualified Data.Map.Strict
import qualified Data.List.NonEmpty

import HauntedHouse.Game.Build.DirectionTemplate
import HauntedHouse.Game.Build.LocationTemplate
import HauntedHouse.Game.Location
import HauntedHouse.Game.Model (GameStateExceptT, GameState (..))
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.World

import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.World

buildFrame :: (GID Location) -> Location -> GameStateExceptT ()
buildFrame locationGID (Location description mObjects exits) = do
  world :: World <- _world' <$> get
  let locationMap' = unLocationMap world
  location <- throwMaybe errmsg
                $ Data.Map.Strict.lookup locationGID locationMap'
  let updatedMap = GIDToDataMapping
                    $ Data.Map.Strict.insert
                        locationGID updatedLocation locationMap'
      updatedLocation = location{ _exits        = exits
                                , _objects      = mObjects
                                , _description  = description}

  modify' (\gs -> gs {_world' = world {_locationMap' = updatedMap}})
  where
    errmsg = "location should have been in this map but wasn't"
    unLocationMap   = _unGIDMapping' . _locationMap'
    
