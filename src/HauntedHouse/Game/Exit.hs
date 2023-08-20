module HauntedHouse.Game.Exit where 
import HauntedHouse.Game.Model.World (Object, GameStateExceptT)
import HauntedHouse.Game.Model.GID (GID)

getExitFromObjectM :: GID Object -> GameStateExceptT () 
getExitFromObjectM _ = pass 

