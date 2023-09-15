module HauntedHouse.Game.Engine.VerbPhraseSeven.Open where
import HauntedHouse.Recognizer (NounPhrase, PrepPhrase)
import HauntedHouse.Game.Engine.Verification (verifySensibilityNPPP)
import HauntedHouse.Clarifier (clarifyingOpenDirectObjectM)
import HauntedHouse.Game.Model.World  
import HauntedHouse.Game.Object (getContainerInterfaceM)

doOpenNPPP :: NounPhrase -> PrepPhrase -> GameStateExceptT ()
doOpenNPPP np pp = do 
  (_,entity) <- verifySensibilityNPPP clarifyingOpenDirectObjectM np pp
  containedIn <- getContainerInterfaceM entity
  _openAction' containedIn


