module HauntedHouse.Game.Engine.VerbPhraseSeven.Open where
import HauntedHouse.Recognizer (NounPhrase, PrepPhrase)
import HauntedHouse.Game.Engine.Verification (verifyExistenceNPPP)
import HauntedHouse.Clarifier (clarifyingOpenDirectObjectM)
import HauntedHouse.Game.Model.World  
import HauntedHouse.Game.Object (getContainerInterfaceM)

doOpenNPPP :: NounPhrase -> PrepPhrase -> GameStateExceptT ()
doOpenNPPP np pp = do 
  (_,entity) <- verifyExistenceNPPP clarifyingOpenDirectObjectM np pp
  containedIn <- getContainerInterfaceM entity
  _openAction' containedIn


