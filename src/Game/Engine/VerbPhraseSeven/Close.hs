module Game.Engine.VerbPhraseSeven.Close where 
import Recognizer (PrepPhrase, NounPhrase)
import Game.Model.World
import Game.Object 
import Game.Engine.Verification (verifySensibilityNPPP)
import Clarifier (clarifyingOpenDirectObjectM)

doCloseNPPP :: NounPhrase -> PrepPhrase -> GameStateExceptT ()
doCloseNPPP np pp = pass {- do
  (_,entity) <- verifySensibilityNPPP clarifyingOpenDirectObjectM np pp
  containedIn <- getContainerInterfaceM entity
  _closeAction' containedIn 
  -}