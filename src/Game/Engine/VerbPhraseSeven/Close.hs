module Game.Engine.VerbPhraseSeven.Close where 
import Recognizer (PrepPhrase, NounPhrase)
import Game.Model.World

doCloseNPPP :: NounPhrase -> PrepPhrase -> GameStateExceptT ()
doCloseNPPP _np _pp = pass 