module HauntedHouse.Game.Location (
  module HauntedHouse.Game.Location
, module HauntedHouse.Game.Location.Exits
, module HauntedHouse.Game.Location.LocationData
, HauntedHouse.Game.Location.LocationMap.LocationMap (..)
) where

import HauntedHouse.Game.GameState.Domain
    ( GameState(_world'), GameStateExceptT )
import HauntedHouse.Game.Location.Exits
import HauntedHouse.Game.Location.LocationData
import HauntedHouse.Game.Location.LocationMap ( LocationMap(..) )
import HauntedHouse.Game.World (World (..))
import Control.Monad.Except ( MonadError(throwError) )
import HauntedHouse.Game.GID (GID)
import qualified Data.Map.Strict

getLocation :: GID Location -> GameStateExceptT Location
getLocation lgid = lookupLocation
  where
    lookupLocation :: GameStateExceptT Location
    lookupLocation = do
      mLocationData <- mLocationT
      case mLocationData of
        Just ld -> pure ld
        Nothing -> throwError ("Couldn't find " <> show lgid)
      where
        unLocationMap = _unLocationMap . _locationMap'
        mLocationT :: GameStateExceptT (Maybe Location)
        mLocationT = do
          world' :: World <- _world' <$> get
          pure $ Data.Map.Strict.lookup lgid (unLocationMap world')