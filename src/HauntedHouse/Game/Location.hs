module HauntedHouse.Game.Location  where

import Data.Map.Strict                (lookup)
import HauntedHouse.Game.Model.GID    (GID(GID))
import HauntedHouse.Game.Model        
        (GameStateExceptT, GameState (_world'))
import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.World  
        (Location (..), World (_locationMap'))
import HauntedHouse.Game.World

--  _locationMap'       :: GIDToDataMapping Location
getLocation :: (GID Location) -> GameStateExceptT (Location) 
getLocation gid = do
  world <- _world' <$> get 
  throwMaybe errmsg $ lookup gid (unLocationMap world)
  where
    unLocationMap = _unGIDMapping' . _locationMap'
    errmsg = "that location wasn;t found"