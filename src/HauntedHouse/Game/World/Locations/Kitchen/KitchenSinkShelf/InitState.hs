module HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkShelf.InitState where
import HauntedHouse.Game.World.InitState (InitStateT)

makeShelf :: InitStateT ()
makeShelf = do  
  makeShelf'
  makeUpperShelfCabinet
  makeLowerShelfCabinet
  where
    makeShelf' :: InitStateT () 
    makeShelf' = pass 

makeUpperShelfCabinet :: InitStateT () 
makeUpperShelfCabinet = pass 

makeLowerShelfCabinet :: InitStateT ()
makeLowerShelfCabinet = pass 