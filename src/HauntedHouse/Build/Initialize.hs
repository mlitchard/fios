module HauntedHouse.Build.Initialize where
{-
import HauntedHouse.Game.Object.Domain (Object)
import HauntedHouse.Game.GID (GID)
import HauntedHouse.Game.Labels (ObjectLabel, LocationLabel)

import HauntedHouse.Build.GameState (defaultLocation, defaultObject)
import HauntedHouse.Build.ObjectTemplate
    ( kitchenCabinetAboveShelfGID,
      kitchenCabinetAboveSinkGID,
      kitchenCabinetBelowShelfGID,
      kitchenShelfGID,
      kitchenSinkGID ) 
import HauntedHouse.Build.LocationTemplate (hallGID, kitchenGID)
import HauntedHouse.Game.Location (Location)
import qualified Data.Map.Strict (Map, fromList)

locationGIDDataPairs :: [(GID Location, Location)]
locationGIDDataPairs = zip locationGIDs locationData' 
  where 
    locationData' = replicate (length locationGIDs) defaultLocation
    locationGIDs :: [GID Location]
    locationGIDs = [ kitchenGID, hallGID]
    
objectGIDDataPairs :: [(GID Object, Object)]
objectGIDDataPairs = zip objectGIDs objectData' 
  where
    objectGIDs = [kitchenCabinetAboveShelfGID
                  , kitchenCabinetAboveSinkGID
                  , kitchenCabinetBelowShelfGID
                  , kitchenShelfGID
                  , kitchenSinkGID
                  ]
    objectData' = replicate (length objectGIDs) defaultObject 

-- objectGIDDataMap :: Data.Map.Strict.Map (GID Object) Object 

-- makeWorld :: WorldT 
-- makeWorld = do 

{-
makeWorld :: InitStateT ()
makeWorld = do
  initLocationMap
  initObjectLabelMap
  initKitchen
--  makeKitchen
--  makeHall
  pass

makeHall :: InitStateT ()
makeHall = pass

initLocationMap :: InitStateT ()
initLocationMap = modify' updateInitWorld
  where
    updateInitWorld :: InitState -> InitState
    updateInitWorld init'@(InitState _ _ _ locations _ world) =
      init'{ _world' = world{HauntedHouse.Build._locationMap'= locations}}

initObjectLabelMap :: InitStateT ()
initObjectLabelMap = modify initObjectLabelMap'
  where
    initObjectLabelMap' :: InitState -> InitState
    initObjectLabelMap' init'@(InitState o _ _ _ _ w) =
      init'{ _world' = updatedWorld}
      where
        updatedWorld :: HauntedHouse.Build.World  
        updatedWorld = w{HauntedHouse.Build._objectLabelMap' = o}
-}
{-
objectLabels' :: [ObjectLabel]
objectLabels' =
  [kitchenSinkLabel
  , kitchenSinkCabinetAboveLabel
  , kitchenSinkCabinetBelowLabel
  , kitchenCabinetAboveShelfLabel
  , kitchenCabinetBelowShelfLabel
  , kitchenShelfLabel]
-}
initLocationLabels :: [LocationLabel] -> [(LocationLabel, [GID Location])]
initLocationLabels locationLabels = map initLoc grouped 
  where
    indexRange = [1 .. (length locationLabels)]

    zipped :: [(LocationLabel, GID Location)]
    zipped = zipWith (\k v -> (k, GID v)) locationLabels indexRange

    grouped :: [Data.List.NonEmpty.NonEmpty (LocationLabel, GID Location)]
    grouped = map Data.List.NonEmpty.fromList
            $ Data.List.groupBy (\(k,_) (k',_) -> k == k')
            $ sort zipped
    
initObjectLabels :: [ObjectLabel] -> [(ObjectLabel, [GID Object])]
initObjectLabels objectLabels = map initObj grouped
  where
    indexRange = [1 .. (length objectLabels)]

    zipped :: [(ObjectLabel, GID Object)]
    zipped = zipWith (\k v -> (k, GID v)) objectLabels indexRange

    grouped :: [Data.List.NonEmpty.NonEmpty (ObjectLabel, GID Object)]
    grouped = map Data.List.NonEmpty.fromList
            $ Data.List.groupBy (\(k,_) (k',_) -> k == k')
            $ sort zipped
-}