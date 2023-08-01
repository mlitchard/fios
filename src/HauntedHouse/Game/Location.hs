module HauntedHouse.Game.Location  where

import Data.Map.Strict                (lookup)
import HauntedHouse.Game.Model
        (GameStateExceptT, GameState (_world',_player')
        , Player (_playerLocation'))
import HauntedHouse.Game.Model.Mapping
    ( GIDToDataMapping(_unGIDToDataMapping') )
import HauntedHouse.Game.Model.World
        (Location (..), World (_locationMap'))
import HauntedHouse.Internal ( throwMaybeM )
import HauntedHouse.Game.Model.GID (GID)

getLocationIdM :: GameStateExceptT (GID Location)
getLocationIdM = do
  _playerLocation' . _player' <$> get

--  _locationMap'       :: GIDToDataMapping Location
getLocationM :: GID Location -> GameStateExceptT Location
getLocationM gid = do
  world <- _world' <$> get
  throwMaybeM errmsg $ lookup gid (unLocationMap world)
  where
    unLocationMap = _unGIDToDataMapping' . _locationMap'
    errmsg = "that location wasn;t found"

