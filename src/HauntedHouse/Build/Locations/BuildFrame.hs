module HauntedHouse.Build.Locations.BuildFrame
  (buildFrame) where

import qualified Data.Map.Strict

import HauntedHouse.Game.Model (GameStateExceptT, GameState (..))
import HauntedHouse.Game.Model.Mapping
    ( GIDToDataMapping(GIDToDataMapping, _unGIDMapping'), Label (Label) )
import HauntedHouse.Game.Model.World
    ( World(_locationMap'), Location(..) )
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.World ( throwMaybe )

buildFrame :: GID Location -> Label Location -> Location -> GameStateExceptT ()
buildFrame locationGID (Label label) (Location _ description mObjects exits) = do
  world :: World <- _world' <$> get
  let locationMap' = unLocationMap world
  location <- throwMaybe errmsg
                $ Data.Map.Strict.lookup locationGID locationMap'
  let updatedMap = GIDToDataMapping
                    $ Data.Map.Strict.insert
                        locationGID updatedLocation locationMap'
      updatedLocation = location{ _title'       = toText label
                                , _exits'       = exits
                                , _objects'     = mObjects
                                , _description' = description}

  modify' (\gs -> gs {_world' = world {_locationMap' = updatedMap}})
  where
    errmsg = "location should have been in this map but wasn't"
    unLocationMap   = _unGIDMapping' . _locationMap'

