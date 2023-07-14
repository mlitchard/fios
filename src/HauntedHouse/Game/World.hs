module HauntedHouse.Game.World where
import HauntedHouse.Game.World.Locations.Kitchen (makeKitchen)
import HauntedHouse.Game.World.WorldState (WorldState)
import HauntedHouse.Game.Location.LocationData (defaultLocationData)
import qualified Data.Map.Strict
import HauntedHouse.Game.Location.Domain (LocationLabel(..))
import HauntedHouse.Tokenizer (Lexeme(..))
import HauntedHouse.Game.GameState (World(_locationMap))
import HauntedHouse.Game.Location.LocationMap (LocationMap(..))

makeWorld :: WorldState 
makeWorld = do
  initLocationMap 
  makeKitchen
  makeHall
  pass

makeHall :: WorldState 
makeHall = pass 

initLocationMap :: WorldState 
initLocationMap = do
  modify (\ws -> ws{_locationMap = initLocationMap'})
  pass
  where
    kitchenLabel = LocationLabel KITCHEN 
    hallLabel    = LocationLabel HALL 
    initLocationMap' = LocationMap $
      Data.Map.Strict.fromList [(kitchenLabel,defaultLocationData)
                                ,(hallLabel,defaultLocationData)]