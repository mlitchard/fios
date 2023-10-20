module Game.Engine.VerbPhraseSeven.Open where
import Recognizer (NounPhrase, PrepPhrase)
import Game.Model.World  
 
doOpenNPPP :: NounPhrase -> PrepPhrase -> GameStateExceptT ()
doOpenNPPP _np _pp = pass

