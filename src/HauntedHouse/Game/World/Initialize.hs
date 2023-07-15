module HauntedHouse.Game.World.Initialize where

import HauntedHouse.Game.Object.Atomic (ObjectLabel)
import HauntedHouse.Game.GID (GID (..))
import HauntedHouse.Game.World.Objects (initOrg)
import Data.List qualified 
import Data.List.NonEmpty qualified 
import HauntedHouse.Game.World.InitState (InitState)
import HauntedHouse.Game.GameState (World(..))
import qualified Data.Map.Strict
import HauntedHouse.Game.World.Labels
import HauntedHouse.Game.Location (LocationMap(..))
import HauntedHouse.Game.Location.LocationData
import HauntedHouse.Game.World.Locations.Kitchen (makeKitchen)

makeWorld :: InitState ()
makeWorld = do
  initLocationMap 
  makeKitchen
  makeHall
  pass

makeHall :: InitState ()
makeHall = pass 

initLocationMap :: InitState ()  
initLocationMap = do
  modify (\(input,ws) -> (,) input (ws{_locationMap = initLocationMap'}))
  pass
  where
    initLocationMap' = LocationMap $
      Data.Map.Strict.fromList [(kitchenLabel,defaultLocationData)
                                ,(hallLabel,defaultLocationData)]
                                
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