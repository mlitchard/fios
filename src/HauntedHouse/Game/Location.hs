module HauntedHouse.Game.Location  where

{-
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

-}