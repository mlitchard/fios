module HauntedHouse.Build.Locations.BuildFrame
  (buildFrame,updateWorldExitMap) where

import qualified Data.Map.Strict

import HauntedHouse.Game.Model (GameStateExceptT, GameState (..))
import HauntedHouse.Game.Model.Mapping
    ( GIDToDataMapping(GIDToDataMapping, _unGIDToDataMapping'), Label (Label) )
import HauntedHouse.Game.Model.World
    ( World(_locationMap', _exitMap'), Location(..), Exit )
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.World ( throwMaybe )

buildFrame :: GID Location -> Label Location -> Location -> GameStateExceptT ()
buildFrame locationGID (Label label) location = do
  world :: World <- _world' <$> get
  let locationMap' = unLocationMap world
  location' <- throwMaybe errmsg
                $ Data.Map.Strict.lookup locationGID locationMap'
  let updatedMap = GIDToDataMapping
                    $ Data.Map.Strict.insert
                        locationGID updatedLocation locationMap'
      updatedLocation = location'
                          { _title'       = title
                          , _description' = description
                          , _objects'     = mObjects
                          , _directions' = directions
                          }

  modify' (\gs -> gs {_world' = world {_locationMap' = updatedMap}})
  where
    errmsg = "location should have been in this map but wasn't"
    unLocationMap = _unGIDToDataMapping' . _locationMap'
    title       = toText label
    description = _description' location
    mObjects    = _objects' location
    directions  = _directions' location

updateWorldExitMap :: GID Exit -> Exit -> GameStateExceptT ()
updateWorldExitMap gid exit = do 
  world <- _world' <$> get 
  let updated = GIDToDataMapping $ Data.Map.Strict.insert gid exit 
                  $ (_unGIDToDataMapping' . _exitMap') world
  modify' (\gs -> gs {_world' = world {_exitMap' = updated}})
