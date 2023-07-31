module HauntedHouse.Game.Location  where

import Data.Map.Strict                (lookup)
import HauntedHouse.Game.Model
        (GameStateExceptT, GameState (_world',_player'), Player (_playerLocation'))
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.World
        (Location (..), World (_locationMap'))
import HauntedHouse.Game.World
import HauntedHouse.Game.Model.GID (GID)

getLocationId :: GameStateExceptT (GID Location)
getLocationId = do
  _playerLocation' . _player' <$> get

--  _locationMap'       :: GIDToDataMapping Location
getLocation :: GID Location -> GameStateExceptT Location
getLocation gid = do
  world <- _world' <$> get
  throwMaybe errmsg $ lookup gid (unLocationMap world)
  where
    unLocationMap = _unGIDToDataMapping' . _locationMap'
    errmsg = "that location wasn;t found"
