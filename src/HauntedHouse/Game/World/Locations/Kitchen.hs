module HauntedHouse.Game.World.Locations.Kitchen where

import HauntedHouse.Game.World.Locations.Kitchen.KitchenSink
import HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.InitState
    ( makeShelf )
import HauntedHouse.Game.World.InitState (InitState)


makeKitchen :: InitState ()
makeKitchen = do
  makeKitchen'
  makeSink 
  makeShelf
    where
      makeKitchen' :: InitState () 
      makeKitchen' = pass 
