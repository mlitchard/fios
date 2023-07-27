module HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.InitState where
import HauntedHouse.Game.World.InitState (WorldT)


makeShelf :: WorldT ()
makeShelf = do  
  makeShelf'
  makeUpperShelfCabinet
  makeLowerShelfCabinet
  where
    makeShelf' :: WorldT () 
    makeShelf' = pass 

makeUpperShelfCabinet :: WorldT () 
makeUpperShelfCabinet = pass 

makeLowerShelfCabinet :: WorldT ()
makeLowerShelfCabinet = pass 