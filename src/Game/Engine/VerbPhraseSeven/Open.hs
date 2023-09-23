module Game.Engine.VerbPhraseSeven.Open where
import Recognizer (NounPhrase, PrepPhrase)
import Game.Engine.Verification (verifySensibilityNPPP)
import Clarifier (clarifyingOpenDirectObjectM)
import Game.Model.World  
import Game.Object 

doOpenNPPP :: NounPhrase -> PrepPhrase -> GameStateExceptT ()
doOpenNPPP np pp = pass {- do 
  (_,entity) <- verifySensibilityNPPP clarifyingOpenDirectObjectM np pp
  containedIn <- getContainerInterfaceM entity
  _openAction' containedIn
-}

