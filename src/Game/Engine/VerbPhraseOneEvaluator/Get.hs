module Game.Engine.VerbPhraseOneEvaluator.Get where 
import Game.Model.World 
  (GameStateExceptT, Object (..), StandardActions (..))
import Recognizer (NounPhrase (..))
import Game.Engine.Verification (verifyAccessabilityNP)
import Game.Model.Display (updateDisplayActionM, showPlayerActionM)

{-
  (_,entity@Object{..}) <- verifyAccessabilityAP ap
  let getM = _get' _standardActions'
  getM entity
-}
doGet :: NounPhrase -> GameStateExceptT ()
doGet np = pass {- do 
  (_,entity@Object{..}) <- verifyAccessabilityNP np
  let getM = _get' _standardActions'
  getM entity 
  updateDisplayActionM showPlayerActionM   

doGet :: NounPhrase -> GameStateExceptT ()
doGet (NounPhrase1 _ np) = do 

doGet (NounPhrase2 (Adjective adj) noun) = do
  pass
doGet _ = throwError "get unfinished"
-}