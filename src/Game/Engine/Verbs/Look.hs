module Game.Engine.Verbs.Look where 
import Game.Model.GID (GID)
import Game.Model.World (GameStateExceptT, Object)
import Recognizer (Preposition)

whichLook :: Preposition -> GID Object -> GameStateExceptT () 
whichLook _ _ = pass 