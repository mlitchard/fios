module HauntedHouse.Game.World.Locations  where
import HauntedHouse.Game.Labels ( LocationLabel, ObjectLabel, ExitLabel )
import HauntedHouse.Game.Location hiding (getLocationData)
import HauntedHouse.Game.World.InitState (InitStateT, InitState (..))
import qualified Data.Map.Strict
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Object (Object (..), ObjectLabelMap (..))
import HauntedHouse.Game.World ( World(..) )
import Control.Monad.Except (MonadError(throwError))
import qualified Data.List.NonEmpty

getLocationData :: LocationLabel -> InitStateT LocationData
getLocationData locationLabel = do
  locationMap' <- _unLocationMap . _locationMap' . _world' <$> get
  let mLocationData = Data.Map.Strict.lookup locationLabel locationMap'
  -- Data.Map.Strict.lookup locationLabel unLocationMap 
  case mLocationData of
    Just locationData -> pure locationData
    Nothing           -> throwError ("could now find" <> show locationLabel)

populateLocation :: LocationLabel 
                      -> (ObjectLabel, GID Object) 
                      -> InitStateT ()
populateLocation locationLabel (objectLabel, gid) = do
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

updateWorld :: LocationMap -> World -> World 
updateWorld locationMap world =
  world{_locationMap' = locationMap}

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
 
