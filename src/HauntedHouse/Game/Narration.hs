module HauntedHouse.Game.Narration where 

import HauntedHouse.Game.Model
import HauntedHouse.Game.Model.World (Location (..))

updateNarration :: Narration -> GameStateExceptT ()
updateNarration narration =  modify' (\g -> g {_narration' = narration})

displayScene :: Location -> GameStateExceptT ()
displayScene _ = do 
  liftIO $ print ("You are in the " :: String)
  pass 


