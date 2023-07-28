module HauntedHouse.Game.Narration where 

import HauntedHouse.Game.Model

updateNarration :: Narration -> GameStateExceptT ()
updateNarration narration =  modify' (\g -> g {_narration' = narration})
