module HauntedHouse.Game.World.Locations.Kitchen where

import HauntedHouse.Game.World.Locations.Kitchen.KitchenSink
import HauntedHouse.Game.World.WorldState (WorldState)
import HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.WorldState

makeKitchen :: WorldState 
makeKitchen = do
  makeKitchen'
  makeSink 
  makeShelf
    where
      makeKitchen' :: WorldState 
      makeKitchen' = pass 
