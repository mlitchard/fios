module HauntedHouse.Game.World.Initialize where

import HauntedHouse.Game.Labels (ObjectLabel, LocationLabel)
import HauntedHouse.Game.GID (GID (..))
import HauntedHouse.Game.World.Objects (initObj, initLoc)
import Data.List qualified
import Data.List.NonEmpty qualified
import HauntedHouse.Game.World.InitState (InitState (..), InitStateT)
import HauntedHouse.Game.World qualified (World(..))
import qualified Data.Map.Strict
import HauntedHouse.Game.World.Labels
import HauntedHouse.Game.Location (LocationMap(..))
import HauntedHouse.Game.World.Locations.Kitchen 
import HauntedHouse.Game.Object.Domain (ObjectLabelMap (..), Object)
import HauntedHouse.Game.Location.LocationData (defaultLocation)
import HauntedHouse.Game.Location (Location)
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
      init'{ _world' = world{HauntedHouse.Game.World._locationMap'= locations}}

initObjectLabelMap :: InitStateT ()
initObjectLabelMap = modify initObjectLabelMap'
  where
    initObjectLabelMap' :: InitState -> InitState
    initObjectLabelMap' init'@(InitState o _ _ _ _ w) =
      init'{ _world' = updatedWorld}
      where
        updatedWorld :: HauntedHouse.Game.World.World  
        updatedWorld = w{HauntedHouse.Game.World._objectLabelMap' = o}
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