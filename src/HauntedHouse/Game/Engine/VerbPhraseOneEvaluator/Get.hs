module HauntedHouse.Game.Engine.VerbPhraseOneEvaluator.Get where 
import HauntedHouse.Game.Model.World 
  (GameStateExceptT, Object (..), StandardActions (..))
import HauntedHouse.Recognizer (NounPhrase (..))
import HauntedHouse.Game.Engine.Verification (verifyAccessabilityNP)

{-
  (_,entity@Object{..}) <- verifyAccessabilityAP ap
  let getM = _get' _standardActions'
  getM entity
-}
doGet :: NounPhrase -> GameStateExceptT ()
doGet np = do 
  (_,entity@Object{..}) <- verifyAccessabilityNP np
  let getM = _get' _standardActions'
  getM entity     
{-
doGet :: NounPhrase -> GameStateExceptT ()
doGet (NounPhrase1 _ np) = do 

doGet (NounPhrase2 (Adjective adj) noun) = do
  pass
doGet _ = throwError "get unfinished"
-}