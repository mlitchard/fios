module HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.InitState where
import HauntedHouse.Game.World.InitState (InitState)

makeShelf :: InitState ()
makeShelf = do  
  makeShelf'
  makeUpperShelfCabinet
  makeLowerShelfCabinet
  where
    makeShelf' :: InitState () 
    makeShelf' = pass 

makeUpperShelfCabinet :: InitState () 
makeUpperShelfCabinet = pass 

makeLowerShelfCabinet :: InitState ()
makeLowerShelfCabinet = pass 