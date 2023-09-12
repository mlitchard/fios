module HauntedHouse.Game.Engine.VerbPhraseSeven.Close where 
import HauntedHouse.Recognizer (PrepPhrase, NounPhrase)
import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Object (getContainerInterfaceM)
import HauntedHouse.Game.Engine.Verification (verifyExistenceNPPP)
import HauntedHouse.Clarifier (clarifyingOpenDirectObjectM)

doCloseNPPP :: NounPhrase -> PrepPhrase -> GameStateExceptT ()
doCloseNPPP np pp = do
  (_,entity) <- verifyExistenceNPPP clarifyingOpenDirectObjectM np pp
  containedIn <- getContainerInterfaceM entity
  _closeAction' containedIn 