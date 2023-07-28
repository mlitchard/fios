module HauntedHouse.Game.Build.Locations  where

-- import HauntedHouse.Tokenizer (Lexeme (..))
import HauntedHouse.Game.Build.Template
import HauntedHouse.Game.Build.Labels
import HauntedHouse.Game.Model.GID
 
{-
getLocation :: LocationLabel
                    -> (NonEmpty (GID Location) -> GID Location)
                    -> InitStateT Location
getLocation locationLabel pickLocationGID = do
  gid <- pickLocationGID <$> getLocationGIDs locationLabel
  getLocation' gid
  where
    getLocation' :: GID Location -> InitStateT Location
    getLocation' gid = do
      locationMap' <- _unLocationMap . _locationMap' . _world' <$> get
      case Data.Map.Strict.lookup gid locationMap' of
        (Just location) -> pure location
        Nothing         -> throwError ("Couldn't find locationdata for " <> show gid)

popLocationGID :: LocationLabel -> InitStateT (GID Location)
popLocationGID lLabel = do
  lmap' <- _unLocationLabelMap . _locationLabelMap' . _world' <$> get
  let mGid = Data.Map.Strict.lookup lLabel lmap'
  case mGid of
    Nothing -> throwError ("could not find " <> show lLabel)
    Just (x :| xs) -> Data.NonEmpty.List.fromList xs
                        & (modify' . updateLocation)
                        >> pure x
  where
    updateLocation ::  Data.List.NonEmpty.NonEmpty (GID Location)
                      -> InitState
                      -> InitState
    updateLocation xs init'@(InitState _ _ locationLabelMap _ _ _)  =
      let updatedMap = LocationLabelMap $ Data.Map.Strict.insert lLabel xs (_unLocationLabelMap locationLabelMap)
      in init'{HauntedHouse.Game.Build.InitState._locationLabelMap = updatedMap}

getLocationGIDs :: LocationLabel -> InitStateT (NonEmpty (GID Location))
getLocationGIDs locationLabel' = do
   locationLabelMap <- _unLocationLabelMap . _locationLabelMap <$> get
   case lookup locationLabel' locationLabelMap of
    Just gid -> pure gid
    Nothing  -> throwError ("Could not find gid for " <> show locationLabel')

-- type BuildObject = Maybe AttachedTo -> Containing -> RelatedObjects -> Object
populateLocation :: LocationLabel
                      -> (LocationLabel, GID Location)
                      -> InitStateT ()
populateLocation _ _ = pass
{-
populateLocationMap locationLabel (objectLabel, gid) = do
  ldata@(LocationData _ objectLabelMap' _) <- getLocationData locationLabel
  world@(World _ _ (LocationMap locationMap)) <- _world' <$> get
  let updatedLocationData = populateLocation' ldata objectLabelMap'
      updatedLocationMap :: LocationMap  
      updatedLocationMap = LocationMap 
                            $ Data.Map.Strict.insert 
                                                locationLabel
                                                updatedLocationData
                                                locationMap
      updatedWorld' = updateWorld updatedLocationMap world     
  modify' (updateInitState updatedWorld')
  where
    addObject :: ObjectLabelMap -> ObjectLabelMap
    addObject (ObjectLabelMap objectLabelMap) =
      ObjectLabelMap
        $ Data.Map.Strict.adjust 
        (Data.List.NonEmpty.insert gid) objectLabel objectLabelMap
    
    updateInitState :: World -> InitState -> InitState
    updateInitState world initState = initState{_world' = world}

    populateLocation' :: LocationData -> ObjectLabelMap -> LocationData 
    populateLocation' ldata objectLabelMap' = 
      ldata{_objectLabelMap = addObject objectLabelMap'}
-}
updateWorld :: LocationMap -> World -> World
updateWorld locationMap world =
  world{_locationMap' = locationMap}
{-
populateExits :: LocationLabel -> ExitMap -> InitStateT ()
populateExits locationLabel exitMap = do
  locationData <- populateExits' <$> getLocationData locationLabel
  world@(World _ _ locationMap) <- _world' <$> get
  let updateLocationMap = updateLocationMap' locationData locationMap
      updatedWorld = updateWorld updateLocationMap world
  modify' (\initState -> initState{_world' = updatedWorld})
  where
    updateLocationMap' :: LocationData -> LocationMap -> LocationMap 
    updateLocationMap' locationData (LocationMap locationMap') = 
      LocationMap 
        $ Data.Map.Strict.insert locationLabel locationData locationMap'
    
    populateExits' :: LocationData -> LocationData 
    populateExits' locationData = 
      locationData{_exits = exitMap} 
-}
-}