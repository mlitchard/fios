module HauntedHouse.Game.World.InitState  where 
import HauntedHouse.Game.Object.Atomic (ObjectLabel)

import HauntedHouse.Game.GID (GID (..))
import HauntedHouse.Game.GameState (World)


type InitState = ExceptT Text (StateT (Init,World) IO) 

data Init = Init {
  input :: [(ObjectLabel, [GID ObjectLabel])]
  , output :: World
}