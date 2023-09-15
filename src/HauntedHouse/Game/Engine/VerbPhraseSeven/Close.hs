module HauntedHouse.Game.Engine.VerbPhraseSeven.Close where 
import HauntedHouse.Recognizer (PrepPhrase, NounPhrase)
import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Object (getContainerInterfaceM)
import HauntedHouse.Game.Engine.Verification (verifySensibilityNPPP)
import HauntedHouse.Clarifier (clarifyingOpenDirectObjectM)

doCloseNPPP :: NounPhrase -> PrepPhrase -> GameStateExceptT ()
doCloseNPPP np pp = do
  (_,entity) <- verifySensibilityNPPP clarifyingOpenDirectObjectM np pp
  containedIn <- getContainerInterfaceM entity
  _closeAction' containedIn 