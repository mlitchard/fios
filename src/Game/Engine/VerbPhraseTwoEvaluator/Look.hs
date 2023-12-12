module Game.Engine.VerbPhraseTwoEvaluator.Look where
import Recognizer.WordClasses
        (PrepPhrase (..), NounPhrase (..))
import Game.Model.World
import Game.Engine.Verification
        (identifyPossiblelObjects, evaluatePossibleObjects, evaluatePossibleObject)
import Control.Monad.Except (MonadError(..))
import Clarifier (clarifyingLookAdverbialObject)
import Game.Engine.Verbs.Look (doLookObject)

evalLookObjectM :: PrepPhrase -> GameStateExceptT ()
evalLookObjectM (PrepPhrase1 prep np) = do
  pdo <- identifyPossiblelObjects np
  case pdo of
    (label,Nothing) -> throwError ("You don't see that here")
    (label,Just (Found fobj)) -> do
                  (FoundObject {..}) <- throwMaybeM "You don't see that here"
                              $ evaluatePossibleObject fobj
                  doLookObject prep _entity'
    (label,Just (Possibles gids)) -> do
                                      let advObjs = catMaybes 
                                                      $ evaluatePossibleObjects gids
                                      --    advObjs' = removeUnpercievables  
                                      clarifyWhich <- _clarifyWhich' <$> ask
                                      clarifyWhich (clarifyingLookAdverbialObject prep) (label, advObjs)
                                      pass

 -- evaluateNounPhrase (clarifyingLookDirectObjectM prep) np 

evalLookObjectM (PrepPhrase2 prep _ adj noun) =  do
  pdo <- identifyPossiblelObjects (NounPhrase2 adj noun)
  case pdo of
    (label,Nothing) -> throwError ("You don't see that here")
    (label,Just (Found fobj)) -> do
                  (FoundObject {..}) <- throwMaybeM "You don't see that here"
                              $ evaluatePossibleObject fobj
                  doLookObject prep _entity'
    (label,Just (Possibles gids)) -> pass

errorSee :: Text -> GameStateExceptT ()
errorSee = print
