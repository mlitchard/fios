module HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.WorldState where
import HauntedHouse.Game.World.WorldState (WorldState)

makeShelf :: WorldState 
makeShelf = do 
  makeShelf'
  makeUpperShelfCabinet
  makeLowerShelfCabinet
  where
    makeShelf' :: WorldState 
    makeShelf' = pass 

makeUpperShelfCabinet :: WorldState 
makeUpperShelfCabinet = pass 

makeLowerShelfCabinet :: WorldState 
makeLowerShelfCabinet = pass 