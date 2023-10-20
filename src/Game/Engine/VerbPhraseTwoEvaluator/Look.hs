module Game.Engine.VerbPhraseTwoEvaluator.Look where
import Recognizer.WordClasses
        (PrepPhrase (..), NounPhrase (..))
import Game.Model.World
import Game.Model.Display
        (showPlayerActionM, showEnvironmentM
        , updateDisplayActionM)
import Tokenizer.Data (Lexeme(..))
import Game.Engine.Verification
        (identifyPossiblelObjects, evaluatePossibleObjects, evaluatePossibleObject)
import Control.Monad.Except (MonadError(..))
import Game.Actions.Look.StandardLook (look)
import Clarifier (clarifyingLookDirectObjectM)
import Game.Scene (tryDisplayF)
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
                                entities <- _entity' 
                                              <<$>> throwMaybeM "You don't see that here" 
                                                    (evaluatePossibleObjects gids)
                                clarifyWhich <- _clarifyWhich' <$> ask
                                clarifyWhich (clarifyingLookDirectObjectM prep) (label, entities)
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
