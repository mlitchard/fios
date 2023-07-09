module HauntedHouse.Game.Location (
  module HauntedHouse.Game.Location,
  HauntedHouse.Game.Location.LocationMap.LocationMap (..),
  HauntedHouse.Game.Location.Domain.LocationName,
) where

import Data.Map.Strict (lookup)
import HauntedHouse.Game.Agent 
import HauntedHouse.Game.GameState.Domain
    ( GameState(_locationMap), GameStateExceptT )
import HauntedHouse.Game.Location.Domain ( LocationName )
import HauntedHouse.Game.Location.LocationData (LocationData (_description))
import HauntedHouse.Game.Location.LocationMap ( LocationMap(..) )
import Control.Monad.Except ( MonadError(throwError) )

getLocationData :: GameStateExceptT LocationData
getLocationData =
  getLocationName >>= lookupLocationData >>= getLocationData'
  where
    lookupLocationData :: LocationName -> GameStateExceptT (Maybe LocationData)
    lookupLocationData locationName =
      (lookup locationName <$> unLocationMap) . _locationMap <$> get

    getLocationData' = \case
      Just ld -> pure ld
      Nothing -> do
        ld <- getLocationName
        
        throwError ("For some reason, I can't find " <> show ld <> ".")

getLocationName :: GameStateExceptT LocationName
getLocationName = _location <$> getAgentData

getDescription :: GameStateExceptT Text
getDescription = _description <$> getLocationData
