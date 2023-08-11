module HauntedHouse.Build.Locations.BuildFrame where

import qualified Data.Map.Strict

import HauntedHouse.Game.Model (GameStateExceptT, GameState (..))
import HauntedHouse.Game.Model.Mapping
    ( GIDToDataMapping(GIDToDataMapping, _unGIDToDataMapping'), Label (Label))
    
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Internal  ( throwMaybeM )
import HauntedHouse.Game.Model.World (Location (..), World (_locationMap' ))

buildLocationMap :: GID Location -> Location -> GameStateExceptT ()
buildLocationMap locationGID location = do
  world :: World <- _world' <$> get
  let locationMap' = unLocationMap world  
      updatedMap = GIDToDataMapping
                    $ Data.Map.Strict.insert
                        locationGID location locationMap'
  modify' (\gs -> gs {_world' = world {_locationMap' = updatedMap}})
  where
    unLocationMap = _unGIDToDataMapping' . _locationMap'
 
    