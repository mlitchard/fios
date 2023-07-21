module HauntedHouse.Game.World.Initialize where

import HauntedHouse.Game.Labels (ObjectLabel)
import HauntedHouse.Game.GID (GID (..))
import HauntedHouse.Game.World.Objects (initOrg)
import Data.List qualified
import Data.List.NonEmpty qualified
import HauntedHouse.Game.World.InitState (InitState (..), InitStateT)
import HauntedHouse.Game.World qualified (World(..))
import qualified Data.Map.Strict
import HauntedHouse.Game.World.Labels
import HauntedHouse.Game.Location (LocationMap(..))
import HauntedHouse.Game.World.Locations.Kitchen (makeKitchen)
import HauntedHouse.Game.Object.Domain (ObjectLabelMap (..))
import HauntedHouse.Game.Location.LocationData (defaultLocationData)

makeWorld :: InitStateT ()
makeWorld = do
  initLocationMap
  initObjectLabelMap
  makeKitchen
  makeHall
  pass

makeHall :: InitStateT ()
makeHall = pass

initLocationMap :: InitStateT ()
initLocationMap = modify' updateInitWorld
  where
    updateInitWorld :: InitState -> InitState
    updateInitWorld init'@(InitState _ _ locations world) =
      init'{ _world' = world{HauntedHouse.Game.World._locationMap'= locations}}

initObjectLabelMap :: InitStateT ()
initObjectLabelMap = modify initObjectLabelMap'
  where
    initObjectLabelMap' :: InitState -> InitState
    initObjectLabelMap' init'@(InitState o _ _ w) =
      init'{ _world' = updatedWorld}
      where
        updatedWorld :: HauntedHouse.Game.World.World  
        updatedWorld = w{HauntedHouse.Game.World._objectLabelMap' = o}

objectLabels' :: [ObjectLabel]
objectLabels' =
  [kitchenSinkLabel
  , kitchenSinkCabinetAboveLabel
  , kitchenSinkCabinetBelowLabel
  , kitchenCabinetAboveShelfLabel
  , kitchenCabinetBelowShelfLabel
  , kitchenShelfLabel]

initObjectLabels :: [ObjectLabel] -> [(ObjectLabel, [GID ObjectLabel])]
initObjectLabels objectLabels = map initOrg grouped
  where
    indexRange = [1 .. (length objectLabels)]

    zipped :: [(ObjectLabel, GID ObjectLabel)]
    zipped = zipWith (\k v -> (k, GID v)) objectLabels indexRange

    grouped :: [Data.List.NonEmpty.NonEmpty (ObjectLabel, GID ObjectLabel)]
    grouped = map Data.List.NonEmpty.fromList
            $ Data.List.groupBy (\(k,_) (k',_) -> k == k')
            $ sort zipped