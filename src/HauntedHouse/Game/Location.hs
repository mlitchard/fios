module HauntedHouse.Game.Location (
  module HauntedHouse.Game.Location
, module HauntedHouse.Game.Location.Exits
, module HauntedHouse.Game.Location.LocationData
, HauntedHouse.Game.Location.LocationMap.LocationMap (..)
) where

import Data.Map.Strict (lookup, Map)
import HauntedHouse.Game.GameState.Domain
    ( GameState(_world), GameStateExceptT )
import HauntedHouse.Game.Labels (LocationLabel)
import HauntedHouse.Game.Location.Exits
import HauntedHouse.Game.Location.LocationData
import HauntedHouse.Game.Location.LocationMap ( LocationMap(..) )
import HauntedHouse.Game.World (World (_locationMap'))
import Control.Monad.Except ( MonadError(throwError) )

getLocationData :: LocationLabel -> GameStateExceptT LocationData
getLocationData = lookupLocationData
  where
    lookupLocationData :: LocationLabel -> GameStateExceptT LocationData
    lookupLocationData locationLabel = do 
      mLocationData <- mLocationDataT
      case mLocationData of 
        Just ld -> pure ld
        Nothing -> throwError ("Couldn't find " <> show locationLabel)
      where
        mLocationDataT :: GameStateExceptT (Maybe LocationData) 
        mLocationDataT = do 
          world' :: World <-  _world <$> get
          let locationMap :: Data.Map.Strict.Map LocationLabel LocationData  
              locationMap = (_unLocationMap . _locationMap') world'
          pure $ Data.Map.Strict.lookup locationLabel locationMap