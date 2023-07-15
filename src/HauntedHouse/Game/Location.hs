module HauntedHouse.Game.Location (
  module HauntedHouse.Game.Location
, module HauntedHouse.Game.Location.Exits 
, module HauntedHouse.Game.Location.LocationData
, HauntedHouse.Game.Location.LocationMap.LocationMap (..)
, module HauntedHouse.Game.Location.Domain
) where

import Data.Map.Strict (lookup)
import HauntedHouse.Game.Agent 
import HauntedHouse.Game.GameState.Domain
    ( GameState(_world), World (_locationMap), GameStateExceptT )
import HauntedHouse.Game.Location.Domain ( LocationLabel (..))
import HauntedHouse.Game.Location.Exits 
import HauntedHouse.Game.Location.LocationData
import HauntedHouse.Game.Location.LocationMap ( LocationMap(..) )
import Control.Monad.Except ( MonadError(throwError) )

getLocationData :: GameStateExceptT LocationData
getLocationData =
  getLocationLabel >>= lookupLocationData >>= getLocationData'
  where
    lookupLocationData :: LocationLabel -> GameStateExceptT (Maybe LocationData)
    lookupLocationData locationLabel =
      (lookup locationLabel <$> unLocationMap) . _locationMap  . _world <$> get

    getLocationData' = \case
      Just ld -> pure ld
      Nothing -> do
        ld <- getLocationLabel
        
        throwError ("For some reason, I can't find " <> show ld <> ".")

getLocationLabel :: GameStateExceptT LocationLabel
getLocationLabel = _location <$> getAgentData

getDescription :: GameStateExceptT Text
getDescription = _description <$> getLocationData
