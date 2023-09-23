module Game.Exit where 
import Game.Model.World (Object, GameStateExceptT)
import Game.Model.GID (GID)

getExitFromObjectM :: GID Object -> GameStateExceptT () 
getExitFromObjectM _ = pass 

